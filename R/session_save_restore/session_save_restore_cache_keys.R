# Canonical plot-cache keys shared by session save and module restore code.
# This unit is intentionally sourced before all save/restore implementations.

# Schema: <module>::<logical_plot_id>::<variant>
.build_canonical_plot_cache_key <- function(module, logical_plot_id = "default", variant = "main") {
  module <- as.character(module %||% "module")[1]
  logical_plot_id <- as.character(logical_plot_id %||% "default")[1]
  variant <- as.character(variant %||% "main")[1]
  paste(module, logical_plot_id, variant, sep = "::")
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
