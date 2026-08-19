# Provenance-first generated-column resolvers for Auto Regex.

AUTO_REGEX_PROVENANCE_OUTCOMES <- c(
  "resolved", "not applicable", "ambiguous", "stale lineage",
  "unsupported origin version", "collision", "missing source",
  "incomplete generated family"
)

auto_regex_provenance_outcome <- function(row, status, origin = NA_character_,
    sources = character(), configuration = "", reason = "") {
  stopifnot(status %in% AUTO_REGEX_PROVENANCE_OUTCOMES)
  data.frame(Row = as.integer(row), Outcome = status, Origin = origin,
    SourceColumns = paste(sources, collapse = "\037"), Configuration = configuration,
    Reason = reason, stringsAsFactors = FALSE, check.names = FALSE)
}

auto_regex_provenance_value <- function(metadata, row, field) {
  if (!field %in% names(metadata)) return(NA_character_)
  value <- metadata[[field]][[row]]
  if (!length(value) || is.na(value)) NA_character_ else as.character(value)
}

auto_regex_resolve_generated_row <- function(metadata, data, row, origin,
    configuration = NULL, allow_fallback = TRUE) {
  column <- as.character(metadata$Column[[row]])
  persisted_origin <- auto_regex_provenance_value(metadata, row, "Provenance Origin")
  if (!is.na(persisted_origin)) {
    if (!identical(persisted_origin, origin))
      return(auto_regex_provenance_outcome(row, "not applicable", persisted_origin))
    version <- suppressWarnings(as.integer(auto_regex_provenance_value(metadata, row,
      "Provenance Version")))
    origin_version <- suppressWarnings(as.integer(auto_regex_provenance_value(metadata, row,
      "Provenance Origin Version")))
    if (is.na(version) || version != DATAWIZARD_PROVENANCE_VERSION ||
        is.na(origin_version) || origin_version != 1L)
      return(auto_regex_provenance_outcome(row, "unsupported origin version", origin))
    sources <- strsplit(auto_regex_provenance_value(metadata, row,
      "Provenance Source Columns"), "\037", fixed = TRUE)[[1L]]
    sources <- sources[nzchar(sources)]
    if (!column %in% names(data))
      return(auto_regex_provenance_outcome(row, "stale lineage", origin, sources,
        reason = "generated column is absent"))
    if (length(sources) && any(!sources %in% names(data)))
      return(auto_regex_provenance_outcome(row, "missing source", origin, sources))
    family <- auto_regex_provenance_value(metadata, row, "Provenance Family")
    persisted_families <- if ("Provenance Family" %in% names(metadata))
      as.character(metadata[["Provenance Family"]]) else rep(NA_character_, nrow(metadata))
    members <- which(!is.na(persisted_families) & persisted_families == family)
    expected <- strsplit(auto_regex_provenance_value(metadata, row,
      "Provenance Generated Columns"), "\037", fixed = TRUE)[[1L]]
    expected <- expected[nzchar(expected)]
    observed <- as.character(metadata$Column[members])
    if (is.na(family) || !nzchar(family) || !length(expected) ||
        !setequal(expected, observed) || any(!expected %in% names(data)))
      return(auto_regex_provenance_outcome(row, "incomplete generated family", origin, sources))
    if (sum(as.character(metadata$Column) == column, na.rm = TRUE) > 1L)
      return(auto_regex_provenance_outcome(row, "collision", origin, sources))
    return(auto_regex_provenance_outcome(row, "resolved", origin, sources,
      auto_regex_provenance_value(metadata, row, "Provenance Configuration")))
  }
  if (!isTRUE(allow_fallback)) return(auto_regex_provenance_outcome(row, "not applicable", origin))
  # Legacy fallback is intentionally exact and configuration-scoped.  It never
  # reads reactive/live module state, which is especially important for XLSX.
  exact <- switch(origin,
    imputation = startsWith(column, "Imputed "),
    batch_correction = startsWith(column, "Batch Corrected "),
    basemean = identical(column, "log2_Basemean"),
    ratio = !is.null(configuration) && column %in% as.character(configuration$generated_columns),
    merge = !is.null(configuration) && column %in% as.character(configuration$generated_columns),
    FALSE)
  if (!isTRUE(exact)) return(auto_regex_provenance_outcome(row, "not applicable", origin))
  auto_regex_provenance_outcome(row, "resolved", origin,
    reason = "legacy exact-name/configuration fallback")
}

auto_regex_resolve_provenance <- function(metadata, data, configurations = list(),
                                           workbook = FALSE) {
  if (!is.data.frame(metadata) || !is.data.frame(data) || !"Column" %in% names(metadata))
    stop("metadata and data must be valid data frames.", call. = FALSE)
  modules <- DATAWIZARD_PROVENANCE_MODULES
  outcomes <- lapply(seq_len(nrow(metadata)), function(row) {
    resolvers <- list(ratio = auto_regex_resolve_ratio_provenance,
      imputation = auto_regex_resolve_imputation_provenance,
      batch_correction = auto_regex_resolve_batch_provenance,
      basemean = auto_regex_resolve_basemean_provenance,
      merge = auto_regex_resolve_merge_provenance)
    attempts <- lapply(modules, function(origin) tryCatch(
      resolvers[[origin]](metadata, data, row,
        configuration = configurations[[origin]], allow_fallback = !isTRUE(workbook)),
      error = function(e) auto_regex_provenance_outcome(row, "not applicable", origin,
        reason = conditionMessage(e))))
    resolved <- Filter(function(x) identical(x$Outcome[[1L]], "resolved"), attempts)
    if (length(resolved) == 1L) return(resolved[[1L]])
    if (length(resolved) > 1L) return(auto_regex_provenance_outcome(row, "ambiguous",
      reason = paste(vapply(resolved, function(x) x$Origin[[1L]], character(1L)), collapse = ",")))
    material <- Filter(function(x) !identical(x$Outcome[[1L]], "not applicable"), attempts)
    if (length(material)) material[[1L]] else attempts[[1L]]
  })
  do.call(rbind, outcomes)
}

auto_regex_resolve_ratio_provenance <- function(...) auto_regex_resolve_generated_row(..., origin = "ratio")
auto_regex_resolve_imputation_provenance <- function(...) auto_regex_resolve_generated_row(..., origin = "imputation")
auto_regex_resolve_batch_provenance <- function(...) auto_regex_resolve_generated_row(..., origin = "batch_correction")
auto_regex_resolve_basemean_provenance <- function(...) auto_regex_resolve_generated_row(..., origin = "basemean")
auto_regex_resolve_merge_provenance <- function(...) auto_regex_resolve_generated_row(..., origin = "merge")

# A contrast map is live session state and therefore cannot identify the data
# frame to which it belongs merely by naming columns that happen to exist.  Its
# mappings all carry the revision of their source data; require the frozen
# orchestration descriptor to carry that same revision before either the map or
# its generated-column names are admitted as evidence.
auto_regex_contrast_collection_matches_source <- function(collection, source_revision) {
  if (!is.list(collection) || !identical(as.integer(collection$version), 1L) ||
      !is.list(collection$mappings) || !length(collection$mappings)) return(FALSE)
  source_revision <- as.character(source_revision %||% character())
  if (length(source_revision) != 1L || is.na(source_revision) ||
      !nzchar(trimws(source_revision))) return(FALSE)
  revisions <- vapply(collection$mappings, function(mapping) {
    value <- if (is.list(mapping)) as.character(mapping$SourceRevision %||% character()) else character()
    if (length(value) == 1L && !is.na(value)) value else NA_character_
  }, character(1L))
  !anyNA(revisions) && all(nzchar(trimws(revisions))) &&
    all(revisions == source_revision)
}

# Complete the provenance-first recovery transaction.  `infer_residual` is the
# legacy ownership search adapter: it receives the complete, unmodified evidence
# table and the row numbers for which it is allowed to propose ownership.  This
# deliberately avoids the tempting (and incorrect) metadata[rows, ] retry, which
# removes negatives and downstream conflict evidence from the search.
auto_regex_recover_residual_ownership <- function(metadata, frozen_rules,
    frozen_rows, infer_residual, condition_target = "Options") {
  frozen_rules <- coerce_contract(frozen_rules)
  frozen_rows <- sort(unique(as.integer(frozen_rows)))
  frozen_rows <- frozen_rows[!is.na(frozen_rows) & frozen_rows >= 1L &
    frozen_rows <= nrow(metadata)]
  residual_rows <- setdiff(seq_len(nrow(metadata)), frozen_rows)
  abstain <- function(code, reason, candidate = NULL, diagnostics = list()) list(
    status = "abstained", authoritative = FALSE, rules = NULL,
    residual_rows = residual_rows,
    abstention = list(code = code, reason = reason),
    diagnostics = c(list(frozen_candidate = frozen_rules,
      residual_candidate = candidate), diagnostics))
  if (!length(residual_rows)) return(list(status = "resolved", authoritative = TRUE,
    rules = frozen_rules, residual_rows = integer(), abstention = NULL,
    diagnostics = list(frozen_candidate = frozen_rules, residual_candidate = NULL)))
  if (!is.function(infer_residual))
    return(abstain("residual_inference_unavailable",
      "The residual legacy inference adapter is unavailable."))

  proposed <- tryCatch(infer_residual(metadata = metadata,
    ownership_rows = residual_rows, frozen_rules = frozen_rules),
    error = function(e) structure(list(message = conditionMessage(e)),
      class = "auto_regex_residual_error"))
  if (inherits(proposed, "auto_regex_residual_error"))
    return(abstain("residual_inference_error", proposed$message))
  if (is.null(proposed) || !is.list(proposed))
    return(abstain("residual_inference_invalid",
      "Residual inference did not return a candidate contract.", proposed))
  if (isTRUE(proposed$limit_reached) || identical(proposed$status, "limit"))
    return(abstain("residual_inference_limit",
      "Residual inference reached its configured resource limit.", proposed))
  candidate <- tryCatch(coerce_contract(proposed$rules %||% proposed),
    error = function(e) e)
  if (inherits(candidate, "error"))
    return(abstain("residual_candidate_invalid", conditionMessage(candidate), proposed))

  # A residual selector may coincide with a frozen selector only when it is the
  # same logical variant.  Any other hit is stealing, even if ordered priority
  # would happen to leave the frozen winner unchanged.
  frozen_application <- apply_content_table(metadata, frozen_rules$table)
  frozen_variants <- chr(attr(frozen_application$metadata, "variant_id", exact = TRUE))
  residual_application <- apply_content_table(metadata, candidate$table)
  residual_variants <- chr(attr(residual_application$metadata, "variant_id", exact = TRUE))
  frozen_keys <- paste(chr(frozen_application$metadata$Content), frozen_variants, sep = "\r")
  residual_keys <- paste(chr(residual_application$metadata$Content), residual_variants, sep = "\r")
  stealing <- frozen_rows[nzchar(chr(residual_application$metadata$Content[frozen_rows])) &
    residual_keys[frozen_rows] != frozen_keys[frozen_rows]]
  if (length(stealing)) return(abstain("residual_candidate_overlap",
    sprintf("Residual ownership candidate matched frozen row(s): %s.",
      paste(stealing, collapse = ",")), candidate,
    list(overlap_rows = stealing)))

  # Frozen rules always precede residual rules; within each block existing
  # priority and row order are stable.  Re-numbering makes this order explicit
  # and serialization-independent.
  combined <- frozen_rules
  residual_table <- candidate$table
  if (nrow(residual_table)) residual_table <- residual_table[
    !paste(chr(residual_table$Content), chr(residual_table$VariantId), sep = "\r") %in%
      paste(chr(frozen_rules$table$Content), chr(frozen_rules$table$VariantId), sep = "\r"),,
    drop = FALSE]
  combined$table <- rbind(frozen_rules$table, residual_table)
  if (nrow(combined$table)) combined$table$Priority <- seq_len(nrow(combined$table))
  combined$condition <- rbind(frozen_rules$condition, candidate$condition)
  combined$ratio <- rbind(frozen_rules$ratio, candidate$ratio)
  combined <- coerce_contract(combined)

  content <- apply_content_table(metadata, combined$table)
  condition_input <- content$metadata
  if (condition_target %in% names(condition_input)) condition_input[[condition_target]] <- ""
  condition <- apply_condition_table(condition_input, combined$condition,
    if (condition_target %in% names(metadata)) chr(metadata[[condition_target]]) else character())
  ratio <- apply_ratio_table(condition$metadata, combined$ratio,
    if ("Numerator" %in% names(metadata)) chr(metadata$Numerator) else character(),
    if ("Denominator" %in% names(metadata)) chr(metadata$Denominator) else character())
  expected_variant <- if ("VariantId" %in% names(metadata)) chr(metadata$VariantId) else {
    value <- residual_variants
    value[frozen_rows] <- frozen_variants[frozen_rows]
    value
  }
  actual_variant <- chr(attr(content$metadata, "variant_id", exact = TRUE))
  expected <- function(field) if (field %in% names(metadata)) chr(metadata[[field]]) else rep("", nrow(metadata))
  actual <- list(Content = chr(content$metadata$Content), VariantId = actual_variant,
    Transformation = chr(content$metadata$Transformation), Options = chr(condition$metadata$Options),
    Numerator = chr(ratio$metadata$Numerator), Denominator = chr(ratio$metadata$Denominator))
  wanted <- list(Content = expected("Content"), VariantId = expected_variant,
    Transformation = expected("Transformation"), Options = expected("Options"),
    Numerator = expected("Numerator"), Denominator = expected("Denominator"))
  mismatch <- unique(unlist(Map(function(a, b) which(a != b), actual, wanted), use.names = FALSE))
  if (length(mismatch)) return(abstain("aggregate_replay_mismatch",
    sprintf("Combined rules did not exactly replay metadata row(s): %s.",
      paste(mismatch, collapse = ",")), candidate,
    list(replay_mismatch_rows = mismatch, combined_candidate = combined)))
  list(status = "resolved", authoritative = TRUE, rules = combined,
    residual_rows = residual_rows, abstention = NULL,
    diagnostics = list(frozen_candidate = frozen_rules,
      residual_candidate = candidate, combined_replay_rows = seq_len(nrow(metadata))))
}
