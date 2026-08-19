# Shared generated-column provenance contract.
#
# Provenance is deliberately stored in ordinary, scalar metadata fields.  It
# consequently survives CSV/XLSX round trips (unlike R attributes), and an
# imported workbook is a self-contained source of lineage.

DATAWIZARD_PROVENANCE_VERSION <- 1L
DATAWIZARD_PROVENANCE_MODULES <- c("ratio", "imputation", "batch_correction",
                                   "basemean", "merge")
DATAWIZARD_PROVENANCE_FIELDS <- c(
  "Provenance Version", "Provenance Origin", "Provenance Origin Version",
  "Provenance Family", "Provenance Generated Columns", "Provenance Source Columns",
  "Provenance Configuration"
)

datawizard_provenance_record <- function(origin, generated_columns,
    source_columns = character(), configuration = "", family = NULL,
    origin_version = 1L, version = DATAWIZARD_PROVENANCE_VERSION) {
  origin <- as.character(origin)[1L]
  generated_columns <- unique(as.character(generated_columns))
  source_columns <- unique(as.character(source_columns))
  if (!origin %in% DATAWIZARD_PROVENANCE_MODULES)
    stop("Unsupported provenance origin.", call. = FALSE)
  if (!length(generated_columns) || anyNA(generated_columns) || any(!nzchar(generated_columns)))
    stop("generated_columns must contain non-empty column names.", call. = FALSE)
  if (is.null(family) || !nzchar(as.character(family)[1L]))
    family <- paste(origin, paste(generated_columns, collapse = "\037"), sep = ":")
  list(version = as.integer(version)[1L], origin = origin,
       origin_version = as.integer(origin_version)[1L], family = as.character(family)[1L],
       generated_columns = generated_columns, source_columns = source_columns,
       configuration = paste(as.character(configuration), collapse = "\037"))
}

datawizard_provenance_metadata_rows <- function(record, template = NULL) {
  stopifnot(is.list(record))
  n <- length(record$generated_columns)
  if (is.null(template)) template <- data.frame(Column = rep(NA_character_, n),
                                                 stringsAsFactors = FALSE)
  if (nrow(template) == 1L && n > 1L) template <- template[rep(1L, n), , drop = FALSE]
  if (nrow(template) != n) stop("Metadata template and generated family differ in size.", call. = FALSE)
  template$Column <- record$generated_columns
  template[[DATAWIZARD_PROVENANCE_FIELDS[1L]]] <- record$version
  template[[DATAWIZARD_PROVENANCE_FIELDS[2L]]] <- record$origin
  template[[DATAWIZARD_PROVENANCE_FIELDS[3L]]] <- record$origin_version
  template[[DATAWIZARD_PROVENANCE_FIELDS[4L]]] <- record$family
  template[[DATAWIZARD_PROVENANCE_FIELDS[5L]]] <- paste(record$generated_columns, collapse = "\037")
  template[[DATAWIZARD_PROVENANCE_FIELDS[6L]]] <- paste(record$source_columns, collapse = "\037")
  template[[DATAWIZARD_PROVENANCE_FIELDS[7L]]] <- record$configuration
  template
}

# Commit a complete generated family to copies, then return both copies.  No
# caller-owned object is mutated before all collision/source/shape checks pass.
datawizard_provenance_commit <- function(data, metadata, columns, record,
                                         metadata_rows = NULL) {
  if (!is.data.frame(data) || !is.data.frame(metadata))
    stop("data and metadata must be data frames.", call. = FALSE)
  if (!is.list(columns) || is.null(names(columns)) || any(!nzchar(names(columns))))
    stop("columns must be a named list.", call. = FALSE)
  if (!identical(names(columns), record$generated_columns))
    stop("Generated columns do not match the provenance family.", call. = FALSE)
  if (any(names(columns) %in% names(data)) ||
      ("Column" %in% names(metadata) && any(names(columns) %in% metadata$Column)))
    stop("Generated column collision.", call. = FALSE)
  if (length(record$source_columns) && any(!record$source_columns %in% names(data)))
    stop("A provenance source column is missing.", call. = FALSE)
  if (any(vapply(columns, length, integer(1L)) != nrow(data)))
    stop("Generated columns must have exactly one value per data row.", call. = FALSE)
  rows <- datawizard_provenance_metadata_rows(record, metadata_rows)
  missing_meta <- setdiff(names(metadata), names(rows))
  for (field in missing_meta) rows[[field]] <- NA
  missing_rows <- setdiff(names(rows), names(metadata))
  for (field in missing_rows) metadata[[field]] <- NA
  rows <- rows[, names(metadata), drop = FALSE]
  next_data <- data
  for (name in names(columns)) next_data[[name]] <- columns[[name]]
  next_metadata <- rbind(metadata, rows)
  rownames(next_metadata) <- NULL
  list(data = next_data, metadata = next_metadata, provenance = record)
}
