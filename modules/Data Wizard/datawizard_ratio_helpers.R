# Pure helpers shared by Auto RegEx inference/replay and Auto-Assign runtime.

#' Locate unambiguous known-sample spans in a column header
#'
#' Matching is exact and case-sensitive.  Longer matches win whenever spans
#' overlap; equal-length overlaps are retained so callers safely abstain rather
#' than choosing an arbitrary interpretation.
#' @param column_name scalar source header
#' @param known_samples known sample labels
#' @return data frame with value, start, end, and is_unique columns
datawizard_known_sample_spans <- function(column_name, known_samples) {
  empty <- data.frame(value = character(), start = integer(), end = integer(),
    is_unique = logical(), stringsAsFactors = FALSE)
  column_name <- as.character(column_name)
  if (length(column_name) != 1L || is.na(column_name) || !nzchar(column_name))
    return(empty)

  known_samples <- unique(as.character(known_samples))
  known_samples <- known_samples[!is.na(known_samples) & nzchar(known_samples)]
  if (!length(known_samples)) return(empty)

  hits <- lapply(known_samples, function(value) {
    width <- nchar(value, type = "chars")
    last <- nchar(column_name, type = "chars") - width + 1L
    if (last < 1L) return(NULL)
    starts <- which(vapply(seq_len(last), function(start) {
      identical(substr(column_name, start, start + width - 1L), value)
    }, logical(1L)))
    if (!length(starts)) return(NULL)
    data.frame(value = value, start = starts, end = starts + width - 1L,
      stringsAsFactors = FALSE)
  })
  hits <- do.call(rbind, hits)
  if (is.null(hits) || !nrow(hits)) return(empty)
  row.names(hits) <- NULL

  widths <- hits$end - hits$start + 1L
  # A candidate loses only to a strictly longer overlapping candidate.  This
  # includes the required longest-at-the-same-start rule without concealing
  # equal-length ambiguity.
  shadowed <- vapply(seq_len(nrow(hits)), function(i) {
    overlap <- hits$start <= hits$end[i] & hits$end >= hits$start[i]
    any(overlap & widths > widths[i])
  }, logical(1L))
  hits <- hits[!shadowed, , drop = FALSE]
  occurrence_count <- table(hits$value)
  hits$is_unique <- unname(occurrence_count[hits$value]) == 1L
  hits <- hits[order(hits$start, -(hits$end - hits$start), hits$value), , drop = FALSE]
  row.names(hits) <- NULL
  hits
}

#' Resolve exactly two unambiguous known-sample components
datawizard_resolve_known_sample_ratio <- function(column_name, known_samples,
                                                   invert = FALSE) {
  spans <- datawizard_known_sample_spans(column_name, known_samples)
  overlaps <- nrow(spans) > 1L && any(spans$start[-1L] <= spans$end[-nrow(spans)])
  if (nrow(spans) != 2L || !all(spans$is_unique) || overlaps) return(NULL)
  values <- spans$value
  if (isTRUE(invert)) values <- rev(values)
  list(numerator = values[[1L]], denominator = values[[2L]])
}
