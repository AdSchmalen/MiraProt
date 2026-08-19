# ============================================================================
# Sub-script: Auto Regex effective-source identity
# Purpose: Normalize and fingerprint metadata, working columns, mappings, and
# workbook boundaries without changing the inference snapshot representation.
# Source-time dependencies: base.
# Call-time dependencies: the %||% compatibility helper supplied by the host.
# ============================================================================

# Return a fixed-size, deterministic checksum without retaining the (potentially
# very large) hexadecimal representation of a serialized data frame.  Two
# independent rolling residues make accidental collisions substantially less
# likely while keeping the stored identity bounded at 35 characters.
auto_regex_compact_signature <- function(value) {
  bytes <- as.integer(serialize(value, NULL, version = 2L))
  modulus <- 2147483629
  rolling <- function(seed, multiplier) {
    hash <- seed
    for (byte in bytes) hash <- (hash * multiplier + byte + 1) %% modulus
    sprintf("%08x", as.integer(hash))
  }
  paste0(length(bytes), ":", rolling(17, 65599), rolling(29, 257))
}

# Excel readers can vary column types while presenting the same effective
# metadata.  Inference consumes these fields as character values, so normalize
# them to that representation before constructing source identity.
auto_regex_normalize_metadata <- function(value) {
  if (!is.data.frame(value)) return(NULL)
  out <- as.data.frame(lapply(value, function(column) {
    normalized <- as.character(column)
    normalized[is.na(normalized)] <- "<NA>"
    normalized
  }), stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- names(value)
  row.names(out) <- NULL
  out
}

auto_regex_column_signature <- function(value) {

  if (!is.data.frame(value)) {
    return(
      auto_regex_compact_signature(
        NULL
      )
    )
  }

  # Auto RegEx learns rules from metadata/header identity, not from the number
  # of observations in the active data frame. Filtering rows or changing data
  # values must therefore not invalidate an otherwise identical rule source.
  auto_regex_compact_signature(
    list(
      columns = names(value)
    )
  )
}

auto_regex_source_descriptor <- function(mode, metadata, working_data = NULL,
                                         workbook = NULL, worksheet = NULL,
                                         mapping = NULL, revisions = NULL) {
  mode <- as.character(mode %||% "current_metadata")[[1L]]
  normalized <- auto_regex_normalize_metadata(metadata)
  mapping <- if (is.null(mapping)) NULL else {
    answer <- as.character(mapping); names(answer) <- names(mapping); answer
  }
  workbook_identity <- if (identical(mode, "excel") && is.list(workbook))
    workbook[intersect(c("name", "size", "type", "path"), names(workbook))] else NULL
  # Current Data Wizard inference already fingerprints the normalized canonical
  # metadata and ordered working columns below. Reactive revision counters and
  # other bookkeeping representations are not additional inference evidence.
  #
  # Excel mode keeps its workbook revision information because workbook/sheet
  # replacement is an actual external source boundary.
  effective_revisions <-
    if (identical(
      mode,
      "current_metadata"
    )) {
      NULL
    } else {
      revisions
    }
  identity <- list(
    mode = mode,
    workbook = workbook_identity,
    worksheet = if (identical(mode, "excel")) as.character(worksheet %||% "") else NULL,
    mapping = mapping,
    metadata = auto_regex_compact_signature(normalized),
    working_columns = auto_regex_column_signature(working_data),
    revisions = effective_revisions
  )
  # Keep the inference snapshot in its original representation.  Only its
  # normalized projection participates in identity, so type-only Excel import
  # differences neither invalidate a run nor alter the values passed to infer.
  list(signature = auto_regex_compact_signature(identity), identity = identity,
       metadata = metadata)
}

auto_regex_source_change_reason <- function(previous, current) {
  if (is.null(previous) || is.null(current)) return("effective source changed")
  old <- previous$identity; new <- current$identity
  if (!identical(old$mode, new$mode)) return("source mode changed")
  if (!identical(old$workbook, new$workbook)) return("workbook changed")
  if (!identical(old$worksheet, new$worksheet)) return("worksheet changed")
  if (!identical(old$mapping, new$mapping)) return("column mapping changed")
  if (!identical(old$metadata, new$metadata)) return("mapped metadata changed")
  if (!identical(old$working_columns, new$working_columns)) return("working-data columns changed")
  if (!identical(old$revisions, new$revisions)) return("source revision changed")
  "effective source changed"
}
