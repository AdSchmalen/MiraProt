# ==============================================================================
# STRING Module - Server Helper Factories
# ==============================================================================
#
# Purpose:
#   Utility factories providing helper functions for dropdown updates, input
#   parsing, STRING database mapping with fallback, and export device setup.
#   All functions are created in a closure over session and debug_log.
#
# Architecture role:
#   initialize_STRING_server_helpers is called inside modSTRINGServer.
#   The returned helpers are passed to other factory functions that need them.
#
# File structure:
#   1. initialize_STRING_server_helpers factory
#   2. update_gsea_dropdown_choices: updates GSEA pathway select input
#   3. update_go_dropdown_choices: updates GO pathway select input
#   4. parse_neighbor_count_input: safely parses neighbor count from input
#   5. STRING organism retrieval, UI update, and selected species parsing
#   6. register_string_organism_observer: update-button observer
#   7. map_proteins_with_fallback: STRINGdb mapping with version fallback
#   8. open_STRING_export_device: opens graphics device for network export
#   9. Return list
#
# Future developers:
#   - All helpers here are stateless closures over session.
#   - debug_log is passed as a factory parameter, not captured from global scope.
# ==============================================================================

initialize_STRING_server_helpers <- function(session, debug_log) {
  update_gsea_dropdown_choices <- function(gsea_data) {
    if (!is.null(gsea_data) && is.data.frame(gsea_data) && nrow(gsea_data) > 0) {
      pathway_names <- if ("Description" %in% colnames(gsea_data)) {
        gsea_data$Description
      } else if ("ID" %in% colnames(gsea_data)) {
        gsea_data$ID
      } else {
        NULL
      }

      if (!is.null(pathway_names)) {
        pathway_names <- pathway_names[!is.na(pathway_names) & pathway_names != ""]
        pathway_names <- unique(pathway_names)

        if (length(pathway_names) > 0) {
          debug_log(paste("Updating GSEA dropdown with", length(pathway_names), "pathways"), 1)
          updateSelectInput(session, "GSEA_STRING", choices = pathway_names, selected = NULL)
        } else {
          debug_log("No valid pathway names after filtering", 1)
        }
      } else {
        debug_log("No valid pathway column found in GSEA data", 1)
      }
    } else {
      updateSelectInput(session, "GSEA_STRING", choices = character(0), selected = character(0))
      debug_log("No valid GSEA data available for dropdown update", 2)
    }
  }

  update_go_dropdown_choices <- function(go_data) {
    if (!is.null(go_data) && is.data.frame(go_data) && nrow(go_data) > 0 && "Description" %in% colnames(go_data)) {
      go_descriptions <- go_data$Description
      go_descriptions <- go_descriptions[!is.na(go_descriptions) & go_descriptions != ""]
      go_descriptions <- unique(go_descriptions)

      debug_log(paste("Updating GO dropdown with", length(go_descriptions), "pathways"), 1)
      updateSelectInput(session, "GO_STRING", choices = go_descriptions, selected = NULL)
    } else {
      updateSelectInput(session, "GO_STRING", choices = character(0), selected = character(0))
      debug_log("No valid GO data available for dropdown update", 2)
    }
  }

  parse_neighbor_count_input <- function(input_values) {
    neighbor_count <- 0
    if ("neighbor_count_STRING" %in% names(input_values)) {
      nc_raw <- input_values$neighbor_count_STRING
      if (!is.null(nc_raw)) {
        neighbor_count <- suppressWarnings(as.integer(nc_raw))
        if (is.na(neighbor_count) || neighbor_count < 0) {
          neighbor_count <- 0
        }
      }
    }
    neighbor_count
  }

  default_string_organism_choices <- function() {
    c("Homo sapiens" = "9606",
      "Mus musculus" = "10090",
      "Rattus norvegicus" = "10116",
      "Drosophila melanogaster" = "7227",
      "Caenorhabditis elegans" = "6239",
      "Saccharomyces cerevisiae" = "4932",
      "Bos taurus" = "9913",
      "Sus scrofa" = "9823",
      "Equus caballus" = "9796")
  }

  current_string_organism_choices <- reactiveVal(default_string_organism_choices())

  get_current_string_organism_choices <- function() {
    choices <- tryCatch(current_string_organism_choices(), error = function(e) NULL)
    if (is.null(choices) || length(choices) == 0L) {
      return(default_string_organism_choices())
    }
    choices
  }

  set_current_string_organism_choices <- function(choices) {
    if (!is.null(choices) && length(choices) > 0L) {
      current_string_organism_choices(choices)
    }
  }

  reset_string_organism_choices <- function() {
    choices <- default_string_organism_choices()
    current_string_organism_choices(choices)
    updateSelectInput(session, "organism_STRING", choices = choices, selected = "9606")
  }

  get_string_organism_label <- function(species_id) {
    choices <- get_current_string_organism_choices()
    matched <- names(choices)[unname(choices) == as.character(species_id)]
    if (length(matched) > 0L && nzchar(matched[1])) {
      return(matched[1])
    }
    default_matched <- names(default_string_organism_choices())[unname(default_string_organism_choices()) == as.character(species_id)]
    if (length(default_matched) > 0L && nzchar(default_matched[1])) {
      return(default_matched[1])
    }
    NA_character_
  }

  normalize_string_version <- function(version_selected = NULL) {
    version_value <- if (!is.null(version_selected) && length(version_selected) == 1L && nzchar(as.character(version_selected))) {
      as.character(version_selected)
    } else {
      "12.0"
    }
    if (!grepl("\\.", version_value)) {
      version_value <- paste0(version_value, ".0")
    }
    version_value
  }

  string_species_download_urls <- function(version_selected = NULL, protocol = "https") {
    version_value <- normalize_string_version(version_selected)
    file_version <- if (identical(version_value, "11.0b")) "11.0" else version_value
    version_host <- paste0("version-", gsub("\\.", "-", version_value), ".string-db.org")

    urls <- c(
      paste0(protocol, "://", version_host, "/download/species.v", file_version, ".txt"),
      paste0(protocol, "://stringdb-static.org/download/species.v", file_version, ".txt"),
      paste0(protocol, "://stringdb-downloads.org/download/species.v", file_version, ".txt")
    )

    api_versions <- tryCatch(
      utils::read.table(
        paste0(protocol, "://string-db.org/api/tsv-no-header/available_api_versions"),
        colClasses = "character",
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
    if (is.data.frame(api_versions) && ncol(api_versions) >= 2L && nrow(api_versions) > 0L) {
      matched <- api_versions[api_versions[[1]] == version_value, 2]
      matched <- matched[!is.na(matched) & nzchar(matched)]
      if (length(matched) > 0L) {
        urls <- c(paste0(matched[1], "/download/species.v", file_version, ".txt"), urls)
      }
    }

    unique(urls)
  }

  read_string_species_file <- function(version_selected = NULL) {
    urls <- string_species_download_urls(version_selected = version_selected)
    last_error <- NULL

    for (species_url in urls) {
      species_data <- tryCatch(
        utils::read.delim(
          species_url,
          header = TRUE,
          quote = "",
          comment.char = "",
          check.names = TRUE,
          stringsAsFactors = FALSE
        ),
        error = function(e) {
          last_error <<- e
          NULL
        }
      )
      if (is.data.frame(species_data) && nrow(species_data) > 0L) {
        attr(species_data, "source_url") <- species_url
        return(species_data)
      }
    }

    if (!is.null(last_error)) {
      stop(last_error)
    }
    stop("No STRING species download URL returned data.")
  }

  match_string_species_column <- function(species_data, candidates) {
    normalized_names <- gsub("[^a-z0-9]", "", tolower(names(species_data)))
    normalized_names <- sub("^x(?=taxon)", "", normalized_names, perl = TRUE)
    normalized_candidates <- gsub("[^a-z0-9]", "", tolower(candidates))

    matched_index <- match(normalized_candidates, normalized_names, nomatch = 0L)
    matched_index <- matched_index[matched_index > 0L]
    if (length(matched_index) == 0L) {
      return(NA_character_)
    }
    names(species_data)[matched_index[1]]
  }

  resolve_string_species_id <- function(species_id_raw) {
    species_id <- suppressWarnings(as.integer(species_id_raw))
    if (length(species_id) != 1L || is.na(species_id) || species_id <= 0L) {
      debug_log("Invalid STRING organism selection; using Homo sapiens (9606)", 1)
      return(9606L)
    }
    species_id
  }

  get_string_organism_choices <- function(version_selected = NULL) {
    species_data <- tryCatch(
      read_string_species_file(version_selected = version_selected),
      error = function(e) e
    )

    if (inherits(species_data, "error")) {
      return(list(
        success = FALSE,
        error = paste(conditionMessage(species_data), "Showing the built-in common organism choices."),
        choices = default_string_organism_choices(),
        organism_count = length(default_string_organism_choices()),
        fallback = TRUE
      ))
    }

    species_data <- as.data.frame(species_data, stringsAsFactors = FALSE)
    if (nrow(species_data) == 0L) {
      return(list(
        success = FALSE,
        error = "STRING returned an empty species file; showing the built-in common organism choices.",
        choices = default_string_organism_choices(),
        organism_count = length(default_string_organism_choices()),
        fallback = TRUE
      ))
    }

    id_column <- match_string_species_column(
      species_data,
      c("taxon_id", "ncbi_taxon_id", "species_id", "taxonomy_id", "species", "ncbiTaxonId")
    )
    name_column <- match_string_species_column(
      species_data,
      c("official_name_NCBI", "official_name", "scientific_name", "species_name", "name", "STRING_name_compact")
    )

    if (is.na(id_column) || is.na(name_column)) {
      return(list(
        success = FALSE,
        error = "STRING species file did not include recognizable species identifier and name columns; showing the built-in common organism choices.",
        choices = default_string_organism_choices(),
        organism_count = length(default_string_organism_choices()),
        fallback = TRUE
      ))
    }

    organism_ids <- as.character(species_data[[id_column]])
    organism_names <- trimws(as.character(species_data[[name_column]]))
    numeric_organism_ids <- suppressWarnings(as.integer(organism_ids))
    valid <- !is.na(organism_ids) & nzchar(organism_ids) &
      !is.na(numeric_organism_ids) & numeric_organism_ids > 0L &
      !is.na(organism_names) & nzchar(organism_names)
    organism_ids <- organism_ids[valid]
    organism_names <- organism_names[valid]

    if (length(organism_ids) == 0L) {
      return(list(
        success = FALSE,
        error = "STRING species file did not contain valid organism entries; showing the built-in common organism choices.",
        choices = default_string_organism_choices(),
        organism_count = length(default_string_organism_choices()),
        fallback = TRUE
      ))
    }

    duplicated_names <- duplicated(organism_names) | duplicated(organism_names, fromLast = TRUE)
    if (any(duplicated_names)) {
      organism_names[duplicated_names] <- paste0(organism_names[duplicated_names], " (", organism_ids[duplicated_names], ")")
    }

    choices <- stats::setNames(organism_ids, organism_names)
    choices <- choices[order(names(choices), na.last = TRUE)]

    list(
      success = TRUE,
      error = NULL,
      choices = choices,
      organism_count = length(choices),
      fallback = FALSE,
      source_url = attr(species_data, "source_url", exact = TRUE)
    )
  }

  update_string_organism_choices <- function(input_values = NULL) {
    current_selection <- tryCatch(input_values$organism_STRING, error = function(e) NULL)
    version_selected <- tryCatch(input_values$version_STRING, error = function(e) NULL)

    result <- get_string_organism_choices(version_selected = version_selected)
    choices <- result$choices
    selected <- if (length(current_selection) == 1L && !is.na(current_selection) && current_selection %in% unname(choices)) {
      current_selection
    } else if ("9606" %in% unname(choices)) {
      "9606"
    } else {
      unname(choices)[1]
    }

    current_string_organism_choices(choices)
    updateSelectInput(session, "organism_STRING", choices = choices, selected = selected)
    result
  }

  register_string_organism_observer <- function(input_values) {
    observeEvent(input_values$update_organisms_STRING, {
      withProgress(message = "Updating STRING organisms...", value = 0, {
        incProgress(0.2, detail = "Fetching organism list from STRINGdb")
        result <- update_string_organism_choices(input_values)

        incProgress(0.8, detail = "Updating UI")
        if (is.list(result) && isTRUE(result$fallback)) {
          error_message <- if (!is.null(result$error)) result$error else "Using the built-in common organism choices."
          debug_log(paste("Using fallback STRING organisms:", error_message), 1)
          showNotification(
            error_message,
            type = "warning",
            duration = 6
          )
        } else if (is.list(result) && isTRUE(result$success)) {
          source_message <- if (!is.null(result$source_url) && nzchar(result$source_url)) {
            paste("from", result$source_url)
          } else {
            ""
          }
          debug_log(paste("Loaded", result$organism_count, "STRING organisms", source_message), 1)
          showNotification(
            paste("Loaded", result$organism_count, "STRING organisms."),
            type = "message",
            duration = 5
          )
        } else {
          error_message <- if (is.list(result) && !is.null(result$error)) result$error else "Unknown error"
          debug_log(paste("Failed to update STRING organisms:", error_message), 1)
          showNotification(
            paste("Failed to update STRING organisms:", error_message),
            type = "error",
            duration = 6
          )
        }
      })
    })
  }

  map_proteins_with_fallback <- function(proteins, version_selected, score_selected, edge_type_selected, species_id_selected = 9606L) {
    string_db_version_used <- version_selected
    species_id_used <- resolve_string_species_id(species_id_selected)

    build_string_db <- function(version) {
      STRINGdb$new(
        version         = version,
        species         = species_id_used,
        score_threshold = score_selected,
        input_directory = "",
        protocol        = 'http',
        network_type    = edge_type_selected
      )
    }

    string_db <- build_string_db(string_db_version_used)

    genes_mapped <- tryCatch(
      string_db$map(proteins, "query", removeUnmappedRows = TRUE),
      error = function(e) {
        if (grepl("nicht definierte Spalten|undefined columns", e$message, ignore.case = TRUE) &&
            string_db_version_used != "11.5") {
          fallback_version <- "11.5"
          debug_log(paste("STRING mapping failed for version", string_db_version_used,
                                 "- retrying with", fallback_version), 1)
          showNotification(
            paste("STRING mapping failed for version", string_db_version_used,
                  "- retrying with", fallback_version),
            type = "warning",
            duration = 6
          )
          string_db_version_used <<- fallback_version
          string_db <<- build_string_db(fallback_version)
          updateSelectInput(session, "version_STRING", selected = fallback_version)
          return(string_db$map(proteins, "query", removeUnmappedRows = TRUE))
        }
        stop(e)
      }
    )

    list(
      genes_mapped = genes_mapped,
      string_db = string_db,
      string_db_version_used = string_db_version_used,
      species_id_used = species_id_used
    )
  }

  open_STRING_export_device <- function(format_selected, file, width_in, height_in, resolution_dpi) {
    switch(
      format_selected,
      "png" = png(file, width = width_in, height = height_in, units = "in", res = resolution_dpi),
      "svg" = svg(file, width = width_in, height = height_in),
      "pdf" = cairo_pdf(file, width = width_in, height = height_in),
      "tiff" = tiff(file, width = width_in, height = height_in, units = "in", res = resolution_dpi),
      "jpeg" = jpeg(file, width = width_in, height = height_in, units = "in", res = resolution_dpi)
    )
  }

  list(
    update_gsea_dropdown_choices = update_gsea_dropdown_choices,
    update_go_dropdown_choices = update_go_dropdown_choices,
    parse_neighbor_count_input = parse_neighbor_count_input,
    default_string_organism_choices = default_string_organism_choices,
    get_current_string_organism_choices = get_current_string_organism_choices,
    set_current_string_organism_choices = set_current_string_organism_choices,
    reset_string_organism_choices = reset_string_organism_choices,
    get_string_organism_label = get_string_organism_label,
    normalize_string_version = normalize_string_version,
    string_species_download_urls = string_species_download_urls,
    read_string_species_file = read_string_species_file,
    resolve_string_species_id = resolve_string_species_id,
    get_string_organism_choices = get_string_organism_choices,
    update_string_organism_choices = update_string_organism_choices,
    register_string_organism_observer = register_string_organism_observer,
    map_proteins_with_fallback = map_proteins_with_fallback,
    open_STRING_export_device = open_STRING_export_device
  )
}
