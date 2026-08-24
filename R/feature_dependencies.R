# Feature-scoped runtime dependency checks.
#
# These packages are installed by the runtime manifests, but are deliberately
# not attached during bootstrap.  Keeping the check here gives old or damaged
# Portable installations a useful diagnostic at the feature boundary.
require_feature_dependency <- function(package, message,
                                       namespace_available = requireNamespace) {
  if (!isTRUE(namespace_available(package, quietly = TRUE))) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

require_pubmed_plot_dependency <- function(
    namespace_available = requireNamespace) {
  require_feature_dependency(
    "europepmc",
    "PubMed citation plots require the 'europepmc' package, but it is not installed.",
    namespace_available
  )
}
