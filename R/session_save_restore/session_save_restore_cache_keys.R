# Canonical plot-cache keys shared by session save and module restore code.
# This unit is intentionally sourced before all save/restore implementations.

# Schema: <module>::<logical_plot_id>::<variant>.  This is deliberately the
# only place that defines or parses that identity: save and restore must not
# apply different NA/default/separator repairs.
.canonical_plot_cache_identity <- function(module = NULL,
                                           logical_plot_id = NULL,
                                           variant = NULL,
                                           key = NULL) {
  fail <- function(outcome, detail) list(
    valid = FALSE, outcome = outcome, detail = detail, key = NULL,
    module = NULL, logical_plot_id = NULL, variant = NULL
  )

  if (!is.null(key)) {
    if (!is.character(key) || length(key) != 1L || is.na(key)) {
      return(fail("na_identity", "canonical key is NA or not a scalar string"))
    }
    # Adjacent separators are a malformed layout, not an implicit empty ID.
    if (grepl("::::", key, fixed = TRUE)) {
      return(fail("malformed_separator_layout", "canonical key contains adjacent '::' separators"))
    }
    # strsplit drops a trailing empty component, so count separators first.
    separators <- gregexpr("::", key, fixed = TRUE)[[1L]]
    separator_count <- if (identical(separators, -1L)) 0L else length(separators)
    if (separator_count != 2L) {
      return(fail("malformed_separator_layout", "canonical key must contain exactly two '::' separators"))
    }
    pieces <- strsplit(key, "::", fixed = TRUE)[[1L]]
    if (length(pieces) != 3L) {
      return(fail("malformed_separator_layout", "canonical key must have exactly three components"))
    }
    module <- pieces[[1L]]
    logical_plot_id <- pieces[[2L]]
    variant <- pieces[[3L]]
  }

  scalar <- function(value) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) return(NULL)
    trimws(value)
  }
  module <- scalar(module)
  logical_plot_id <- scalar(logical_plot_id)
  variant <- scalar(variant)
  if (is.null(module) || is.null(logical_plot_id) || is.null(variant)) {
    return(fail("na_identity", "module, logical plot ID, and variant must be scalar non-NA strings"))
  }
  if (!nzchar(module) || !nzchar(logical_plot_id) || !nzchar(variant)) {
    return(fail("empty_identity_component", "module, logical plot ID, and variant must be non-empty"))
  }
  if (any(grepl("::", c(module, logical_plot_id, variant), fixed = TRUE))) {
    return(fail("malformed_separator_layout", "identity components may not contain '::'"))
  }

  module <- tolower(module)
  canonical <- paste(module, logical_plot_id, variant, sep = "::")
  list(valid = TRUE, outcome = "valid", detail = NULL, key = canonical,
       module = module, logical_plot_id = logical_plot_id, variant = variant)
}

.build_canonical_plot_cache_key <- function(module, logical_plot_id = "default", variant = "main") {
  identity <- .canonical_plot_cache_identity(module, logical_plot_id, variant)
  if (!isTRUE(identity$valid)) {
    stop("Invalid canonical plot cache identity [", identity$outcome, "]: ",
         identity$detail, call. = FALSE)
  }
  identity$key
}

# Historical PCA cache maps used the bare logical plot id.  Keep that lookup
# explicit so legacy compatibility is diagnosable rather than confused with a
# missing canonical helper.
.legacy_plot_cache_key <- function(logical_plot_id = "default") {
  as.character(logical_plot_id %||% "default")[1]
}

.serialize_plot_variant_spec <- function(spec = NULL) {
  if (is.null(spec)) return("main")
  if (!is.list(spec)) return(as.character(spec)[1])
  nms <- names(spec) %||% character()
  if (length(spec) == 0L || length(nms) == 0L) return("main")
  ord <- order(nms)
  nms <- nms[ord]
  vals <- spec[ord]
  parts <- vapply(seq_along(vals), function(i) {
    val_txt <- paste(as.character(vals[[i]] %||% "none"), collapse = "~")
    paste0(nms[[i]], "=", val_txt)
  }, character(1L))
  paste(parts, collapse = "__")
}
