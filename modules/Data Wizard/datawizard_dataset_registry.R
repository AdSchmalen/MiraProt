# ============================================================================
# Module/Sub-script: modules/Data Wizard/datawizard_dataset_registry.R
# Purpose:
#   Central dataset registry for Data Wizard datasets by role and revision.
# ============================================================================

#' Data Wizard dataset roles managed by the central registry.
datawizard_dataset_roles <- c(
  "primary_original",
  "primary_raw",
  "primary_working",
  "primary_filtered",
  "primary_final",
  "secondary_original",
  "secondary_working",
  "metadata_working",
  "metadata_final"
)

#' Create a compact column signature for a tabular object.
#'
#' @param data Dataset-like object.
#' @return Character signature describing column names and classes.
create_datawizard_column_signature <- function(data) {
  if (is.null(data) || is.null(names(data))) {
    return(character(0))
  }

  vapply(names(data), function(column_name) {
    column_class <- paste(class(data[[column_name]]), collapse = "/")
    paste0(column_name, ":", column_class)
  }, character(1), USE.NAMES = FALSE)
}

#' Create a central Data Wizard dataset registry.
#'
#' The registry stores immutable revision entries by dataset role. Each entry
#' includes identifying metadata, dimensions, a column signature, optional source
#' metadata, and either an in-memory data frame or a lazy reference.
#'
#' @return List of registry operation functions.
create_datawizard_dataset_registry <- function() {
  registry_env <- new.env(parent = emptyenv())
  registry_env$entries <- setNames(vector("list", length(datawizard_dataset_roles)), datawizard_dataset_roles)
  registry_env$dataset_counter <- 0L

  validate_role <- function(role) {
    if (!is.character(role) || length(role) != 1L || !(role %in% datawizard_dataset_roles)) {
      stop("Unknown Data Wizard dataset role: ", paste(role, collapse = ", "), call. = FALSE)
    }
    invisible(role)
  }

  next_dataset_id <- function(role) {
    registry_env$dataset_counter <- registry_env$dataset_counter + 1L
    paste0(role, "_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", registry_env$dataset_counter)
  }

  next_revision <- function(role) {
    validate_role(role)
    length(registry_env$entries[[role]]) + 1L
  }

  make_entry <- function(role, data = NULL, lazy_ref = NULL, source_metadata = NULL,
                         dataset_id = NULL, revision = NULL) {
    validate_role(role)
    if (is.null(dataset_id)) {
      dataset_id <- next_dataset_id(role)
    }
    if (is.null(revision)) {
      revision <- next_revision(role)
    }

    list(
      dataset_id = dataset_id,
      role = role,
      revision = as.integer(revision),
      dimensions = if (is.null(data)) NULL else dim(data),
      column_signature = create_datawizard_column_signature(data),
      source_metadata = source_metadata,
      data = data,
      lazy_ref = lazy_ref,
      created_at = Sys.time()
    )
  }

  set <- function(role, data = NULL, source_metadata = NULL, lazy_ref = NULL,
                  dataset_id = NULL, revision = NULL, allow_original_update = FALSE) {
    validate_role(role)
    original_roles <- c("primary_original", "secondary_original")
    if (role %in% original_roles && length(registry_env$entries[[role]]) > 0L &&
        !isTRUE(allow_original_update)) {
      return(invisible(registry_env$entries[[role]][[length(registry_env$entries[[role]])]]))
    }

    entry <- make_entry(
      role = role,
      data = data,
      lazy_ref = lazy_ref,
      source_metadata = source_metadata,
      dataset_id = dataset_id,
      revision = revision
    )
    registry_env$entries[[role]][[length(registry_env$entries[[role]]) + 1L]] <- entry
    invisible(entry)
  }

  get_latest_entry <- function(role) {
    validate_role(role)
    role_entries <- registry_env$entries[[role]]
    if (length(role_entries) == 0L) {
      return(NULL)
    }
    role_entries[[length(role_entries)]]
  }

  get_latest_data <- function(role) {
    entry <- get_latest_entry(role)
    if (is.null(entry)) {
      return(NULL)
    }
    entry$data
  }

  list(
    roles = datawizard_dataset_roles,
    set = set,
    get_latest_entry = get_latest_entry,
    get_latest_data = get_latest_data,
    get_entries = function(role) {
      validate_role(role)
      registry_env$entries[[role]]
    },
    snapshot = function() {
      registry_env$entries
    },
    clear = function(role = NULL) {
      if (is.null(role)) {
        registry_env$entries <- setNames(vector("list", length(datawizard_dataset_roles)), datawizard_dataset_roles)
      } else {
        validate_role(role)
        registry_env$entries[[role]] <- list()
      }
      invisible(TRUE)
    }
  )
}
