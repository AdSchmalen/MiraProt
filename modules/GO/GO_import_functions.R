# GO Module Excel Import Functions
# Complete implementation for importing GO analysis results from Excel exports

#' Validate Excel file structure for GO import
#' @param file_path path to Excel file
#' @param debug_log debug logging function
#' @return list with validation results and sheet info
validate_go_excel_structure <- function(file_path, debug_log) {
  debug_log("Starting GO Excel structure validation", level = 1)

  validation_result <- list(
    valid = FALSE,
    has_go_sheet = FALSE,
    has_original_data = FALSE,
    has_metadata = FALSE,
    sheet_names = character(0),
    error_message = NULL
  )

  tryCatch({
    # Check if file exists and is readable
    if (!file.exists(file_path)) {
      validation_result$error_message <- "Excel file does not exist"
      return(validation_result)
    }

    # Get sheet names
    sheet_names <- openxlsx::getSheetNames(file_path)
    validation_result$sheet_names <- sheet_names

    debug_log(paste("Found sheets:", paste(sheet_names, collapse = ", ")), level = 2)

    # Check for required sheets
    validation_result$has_go_sheet <- any(grepl("GO_Analysis|GO.Analysis", sheet_names, ignore.case = TRUE))
    validation_result$has_original_data <- any(grepl("Original_Data|Original.Data", sheet_names, ignore.case = TRUE))
    validation_result$has_metadata <- any(grepl("Metadata", sheet_names, ignore.case = TRUE))

    if (!validation_result$has_go_sheet) {
      validation_result$error_message <- "No GO Analysis sheet found in Excel file"
      return(validation_result)
    }

    # Validate GO sheet structure
    go_sheet_name <- sheet_names[grepl("GO_Analysis|GO.Analysis", sheet_names, ignore.case = TRUE)][1]
    go_data <- openxlsx::read.xlsx(file_path, sheet = go_sheet_name, startRow = 1)

    if (nrow(go_data) == 0) {
      validation_result$error_message <- "GO Analysis sheet is empty"
      return(validation_result)
    }

    # Check for required columns
    required_cols <- c("GO_ID", "Description", "P_Value", "Adjusted_P_Value")
    missing_cols <- required_cols[!required_cols %in% colnames(go_data)]

    if (length(missing_cols) > 0) {
      validation_result$error_message <- paste("Missing required columns:", paste(missing_cols, collapse = ", "))
      return(validation_result)
    }

    validation_result$valid <- TRUE
    debug_log("Excel structure validation passed", level = 1)

  }, error = function(e) {
    validation_result$error_message <- paste("Error reading Excel file:", e$message)
    debug_log(paste("Validation error:", e$message), level = 1)
  })

  return(validation_result)
}

#' Import GO analysis results from Excel
#' @param file_path path to Excel file
#' @param debug_log debug logging function
#' @return list with imported data or NULL if failed
import_go_results_from_excel <- function(file_path, debug_log) {
  debug_log("Starting GO results import from Excel", level = 1)

  tryCatch({
    # Validate file structure first
    validation <- validate_go_excel_structure(file_path, debug_log)

    if (!validation$valid) {
      showNotification(paste("Import failed:", validation$error_message), type = "error", duration = 5)
      return(NULL)
    }

    # Find GO Analysis sheet
    sheet_names <- validation$sheet_names
    go_sheet_name <- sheet_names[grepl("GO_Analysis|GO.Analysis", sheet_names, ignore.case = TRUE)][1]

    debug_log(paste("Reading GO data from sheet:", go_sheet_name), level = 1)

    # Read GO analysis data
    go_excel_data <- openxlsx::read.xlsx(file_path, sheet = go_sheet_name, startRow = 1)

    debug_log(paste("Imported", nrow(go_excel_data), "GO terms"), level = 1)

    # Convert Excel format back to internal format
    go_internal_data <- convert_excel_to_internal_go_format(go_excel_data, debug_log)

    if (is.null(go_internal_data)) {
      showNotification("Failed to convert GO data to internal format", type = "error", duration = 5)
      return(NULL)
    }

    # Try to import associated data if available
    original_data <- NULL
    metadata <- NULL

    if (validation$has_original_data) {
      tryCatch({
        original_sheet <- sheet_names[grepl("Original_Data|Original.Data", sheet_names, ignore.case = TRUE)][1]
        original_data <- openxlsx::read.xlsx(file_path, sheet = original_sheet, startRow = 1)
        debug_log(paste("Imported original data:", nrow(original_data), "rows"), level = 2)
      }, error = function(e) {
        debug_log(paste("Could not import original data:", e$message), level = 2)
      })
    }

    if (validation$has_metadata) {
      tryCatch({
        metadata_sheet <- sheet_names[grepl("Metadata", sheet_names, ignore.case = TRUE)][1]
        metadata <- openxlsx::read.xlsx(file_path, sheet = metadata_sheet, startRow = 1)
        debug_log(paste("Imported metadata:", nrow(metadata), "rows"), level = 2)
      }, error = function(e) {
        debug_log(paste("Could not import metadata:", e$message), level = 2)
      })
    }

    import_result <- list(
      go_results = go_internal_data,
      original_data = original_data,
      metadata = metadata,
      import_source = file_path,
      import_time = Sys.time()
    )

    showNotification("GO analysis results imported successfully", type = "message", duration = 3)
    debug_log("GO import completed successfully", level = 1)

    return(import_result)

  }, error = function(e) {
    error_msg <- paste("Error importing GO results:", e$message)
    debug_log(error_msg, level = 1)
    showNotification(error_msg, type = "error", duration = 5)
    return(NULL)
  })
}

#' Convert Excel GO format back to internal enrichResult-like format
#' @param excel_data GO data from Excel
#' @param debug_log debug logging function
#' @return data frame in internal format
convert_excel_to_internal_go_format <- function(excel_data, debug_log) {
  debug_log("Converting Excel GO format to internal format", level = 2)

  tryCatch({
    internal_format <- data.frame(
      ID          = as.character(excel_data$GO_ID),
      Description = as.character(excel_data$Description),
      GeneRatio   = if ("Gene_Ratio" %in% colnames(excel_data)) as.character(excel_data$Gene_Ratio) else rep("", nrow(excel_data)),
      BgRatio     = rep("", nrow(excel_data)),
      pvalue      = if ("P_Value" %in% colnames(excel_data)) as.numeric(excel_data$P_Value) else rep(1, nrow(excel_data)),
      p.adjust    = as.numeric(excel_data$Adjusted_P_Value),
      qvalue      = if ("Q_Value" %in% colnames(excel_data)) as.numeric(excel_data$Q_Value) else as.numeric(excel_data$Adjusted_P_Value),
      geneID      = if ("Enriched_Genes" %in% colnames(excel_data)) {
        sapply(excel_data$Enriched_Genes, function(x) {
          if (is.na(x) || x == "") return("")
          gsub(",\\s*", "/", as.character(x))
        })
      } else {
        rep("", nrow(excel_data))
      },
      Count       = if ("Gene_Count" %in% colnames(excel_data)) as.integer(excel_data$Gene_Count) else rep(0L, nrow(excel_data)),
      stringsAsFactors = FALSE
    )

    # Remove Zeilen ohne p.adjust
    internal_format <- internal_format[!is.na(internal_format$p.adjust), ]

    # Gene-Sets parsen und leere entfernen
    gene_sets <- lapply(internal_format$geneID, function(x) {
      g <- unlist(strsplit(x, "/"))
      g <- trimws(g)
      g[g != ""]
    })
    keep <- vapply(gene_sets, function(gs) length(gs) > 0, logical(1))
    internal_format <- internal_format[keep, , drop = FALSE]
    gene_sets <- gene_sets[keep]
    internal_format$Count <- vapply(gene_sets, length, integer(1))

    debug_log(paste("Converted", nrow(internal_format), "GO terms to internal format after removing empty gene sets"), level = 2)
    return(internal_format)

  }, error = function(e) {
    debug_log(paste("Error converting Excel format:", e$message), level = 1)
    return(NULL)
  })
}

#' Classify GO domains from data (BP/MF/CC)
#' @param go_ids vector of GO IDs
#' @param descriptions vector of GO descriptions
#' @param debug_log debug logging function
#' @return vector of domain classifications
classify_go_domains_from_data <- function(go_ids, descriptions, debug_log) {
  domains <- character(length(go_ids))

  # Simple keyword-based classification (since we don't have GO.db access for imported data)
  for (i in seq_along(descriptions)) {
    desc_lower <- tolower(descriptions[i])

    # Biological Process keywords
    if (grepl("process|pathway|regulation|signaling|response|transport|metabolism|biosynthesis|catabolism|development|differentiation|proliferation|apoptosis|cycle", desc_lower)) {
      domains[i] <- "Biological Process"
    }
    # Molecular Function keywords
    else if (grepl("activity|binding|catalytic|transporter|receptor|kinase|phosphatase|transferase|hydrolase|oxidoreductase|ligase|lyase|isomerase", desc_lower)) {
      domains[i] <- "Molecular Function"
    }
    # Cellular Component keywords
    else if (grepl("component|complex|membrane|organelle|nucleus|cytoplasm|mitochondria|ribosome|endoplasmic|golgi|lysosome|peroxisome|cytoskeleton|vesicle|extracellular", desc_lower)) {
      domains[i] <- "Cellular Component"
    }
    # Default to BP if unclear
    else {
      domains[i] <- "Biological Process"
    }
  }

  debug_log(paste("Domain classification:",
                  sum(domains == "Biological Process"), "BP,",
                  sum(domains == "Molecular Function"), "MF,",
                  sum(domains == "Cellular Component"), "CC"), 2)

  return(domains)
}

#' Build intermediate groups from data (same logic as original)
#' @param terms vector of GO term descriptions
#' @param go_ids vector of GO IDs
#' @param p_values vector of p-values
#' @param domain domain name
#' @param debug_log debug logging function
#' @return intermediate group structure
build_intermediate_groups_from_data <- function(terms, go_ids, p_values, domain, debug_log) {

  # Create functional groupings based on domain
  if (domain == "Biological Process") {
    groups <- group_biological_processes(terms, go_ids, p_values, debug_log)
  } else if (domain == "Molecular Function") {
    groups <- group_molecular_functions(terms, go_ids, p_values, debug_log)
  } else if (domain == "Cellular Component") {
    groups <- group_cellular_components(terms, go_ids, p_values, debug_log)
  } else {
    # Fallback: alphabetical grouping
    groups <- group_alphabetically_imported(terms, go_ids, p_values, debug_log)
  }

  return(groups)
}

#' Simple grouping functions (simplified versions of the original complex ones)
group_biological_processes <- function(terms, go_ids, p_values, debug_log) {
  groups <- list()

  # Simple keyword-based BP grouping
  bp_categories <- list(
    "Metabolic Processes" = c("metabolic", "metabolism", "biosynthesis", "catabolism"),
    "Cell Cycle & Death" = c("cell cycle", "apoptosis", "cell death", "proliferation"),
    "Development" = c("development", "differentiation", "morphogenesis", "organogenesis"),
    "Transport & Localization" = c("transport", "localization", "secretion", "import", "export"),
    "Signaling" = c("signaling", "signal", "response", "transduction", "cascade"),
    "Regulation" = c("regulation", "negative regulation", "positive regulation", "control"),
    "Other Processes" = c()
  )

  for (i in seq_along(terms)) {
    term_lower <- tolower(terms[i])
    assigned <- FALSE

    for (category in names(bp_categories)[1:6]) { # Exclude "Other"
      keywords <- bp_categories[[category]]
      if (any(sapply(keywords, function(kw) grepl(kw, term_lower, fixed = TRUE)))) {
        if (is.null(groups[[category]])) groups[[category]] <- list()
        groups[[category]][[terms[i]]] <- ""
        assigned <- TRUE
        break
      }
    }

    if (!assigned) {
      if (is.null(groups[["Other Processes"]])) groups[["Other Processes"]] <- list()
      groups[["Other Processes"]][[terms[i]]] <- ""
    }
  }

  return(groups[sapply(groups, length) > 0])
}

group_molecular_functions <- function(terms, go_ids, p_values, debug_log) {
  groups <- list()

  mf_categories <- list(
    "Binding" = c("binding", "receptor"),
    "Catalytic Activity" = c("activity", "catalytic", "kinase", "phosphatase", "transferase"),
    "Transporters" = c("transporter", "channel", "carrier"),
    "Other Functions" = c()
  )

  for (i in seq_along(terms)) {
    term_lower <- tolower(terms[i])
    assigned <- FALSE

    for (category in names(mf_categories)[1:3]) {
      keywords <- mf_categories[[category]]
      if (any(sapply(keywords, function(kw) grepl(kw, term_lower, fixed = TRUE)))) {
        if (is.null(groups[[category]])) groups[[category]] <- list()
        groups[[category]][[terms[i]]] <- ""
        assigned <- TRUE
        break
      }
    }

    if (!assigned) {
      if (is.null(groups[["Other Functions"]])) groups[["Other Functions"]] <- list()
      groups[["Other Functions"]][[terms[i]]] <- ""
    }
  }

  return(groups[sapply(groups, length) > 0])
}

group_cellular_components <- function(terms, go_ids, p_values, debug_log) {
  groups <- list()

  cc_categories <- list(
    "Nucleus" = c("nucleus", "nuclear", "chromatin", "nucleolus"),
    "Cytoplasm" = c("cytoplasm", "cytosol", "cytoplasmic", "cytoskeleton"),
    "Membrane" = c("membrane", "plasma membrane", "integral", "transmembrane"),
    "Organelles" = c("mitochondria", "endoplasmic reticulum", "golgi", "lysosome"),
    "Other Components" = c()
  )

  for (i in seq_along(terms)) {
    term_lower <- tolower(terms[i])
    assigned <- FALSE

    for (category in names(cc_categories)[1:4]) {
      keywords <- cc_categories[[category]]
      if (any(sapply(keywords, function(kw) grepl(kw, term_lower, fixed = TRUE)))) {
        if (is.null(groups[[category]])) groups[[category]] <- list()
        groups[[category]][[terms[i]]] <- ""
        assigned <- TRUE
        break
      }
    }

    if (!assigned) {
      if (is.null(groups[["Other Components"]])) groups[["Other Components"]] <- list()
      groups[["Other Components"]][[terms[i]]] <- ""
    }
  }

  return(groups[sapply(groups, length) > 0])
}

group_alphabetically_imported <- function(terms, go_ids, p_values, debug_log) {
  groups <- list()

  for (i in seq_along(terms)) {
    first_letter <- toupper(substr(trimws(terms[i]), 1, 1))
    if (!first_letter %in% LETTERS) first_letter <- "Other"

    group_name <- paste("Terms", first_letter)
    if (is.null(groups[[group_name]])) groups[[group_name]] <- list()
    groups[[group_name]][[terms[i]]] <- ""
  }

  return(groups[order(names(groups))])
}


#' Create REAL enrichResult S4 object from imported data
#' @param imported_go_data data frame with imported GO results
#' @param debug_log debug logging function
#' @return real S4 enrichResult object
create_real_enrichresult_object <- function(imported_go_data, debug_log) {
  debug_log("=== CREATING REAL S4 ENRICHRESULT OBJECT ===", 1)

  tryCatch({
    if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
      debug_log("clusterProfiler not available - using fallback", 1)
      return(NULL)
    }

    # Gene-Sets parsen
    gene_sets <- setNames(
      lapply(imported_go_data$geneID, function(x) {
        g <- unlist(strsplit(x, "/"))
        g <- trimws(g)
        g[g != ""]
      }),
      imported_go_data$ID
    )
    keep <- vapply(gene_sets, function(gs) length(gs) > 0, logical(1))
    gene_sets <- gene_sets[keep]

    # Ergebnis-Daten synchron filtern
    imported_go_data <- imported_go_data[keep, , drop = FALSE]
    imported_go_data$Count <- vapply(gene_sets, length, integer(1))

    gene_vec <- sort(unique(unlist(gene_sets)))

    result_obj <- methods::new("enrichResult")
    result_obj@result      <- imported_go_data
    result_obj@pvalueCutoff <- 0.05
    result_obj@qvalueCutoff <- 0.2
    result_obj@gene        <- gene_vec
    result_obj@universe    <- gene_vec
    result_obj@geneSets    <- gene_sets
    result_obj@organism    <- "UNKNOWN"
    result_obj@keytype     <- "UNKNOWN"
    result_obj@ontology    <- "UNKNOWN"
    result_obj@readable    <- FALSE

    debug_log(paste("Real S4 enrichResult created with", nrow(result_obj@result), "terms and", length(gene_vec), "genes"), 1)
    return(result_obj)

  }, error = function(e) {
    debug_log(paste("Failed to create real S4 object:", e$message), 1)
    return(NULL)
  })
}

#' Create GO tree structure for imported data - IDENTICAL to calculated version
#' @param imported_go_data data frame with imported GO results
#' @param debug_log debug logging function
#' @return hierarchical tree structure IDENTICAL to calculated results
create_hierarchical_tree_from_imported_data <- function(imported_go_data, debug_log) {
  debug_log("Creating hierarchical GO tree structure (identical to calculated)", 1)

  tryCatch({
    if (nrow(imported_go_data) == 0) {
      debug_log("No results found - returning empty tree", 1)
      return(list("No terms found" = ""))
    }

    # Sort by p.adjust and use ALL terms (same as original)
    imported_go_data <- imported_go_data[order(imported_go_data$p.adjust), ]
    max_terms <- min(100, nrow(imported_go_data))  # Same limit as original
    imported_go_data <- imported_go_data[1:max_terms, ]

    debug_log(paste("Processing", nrow(imported_go_data), "GO terms for hierarchy"), 1)

    # Extract essential data (same structure as original)
    go_ids <- imported_go_data$ID
    descriptions <- imported_go_data$Description
    p_values <- imported_go_data$p.adjust

    # Step 1: Classify terms by GO domain (SAME function as original)
    debug_log("Step 1: Classifying GO domains", 2)
    domains <- classify_go_domains_imported(go_ids, descriptions, debug_log)

    # Step 2: Create hierarchical structure (SAME as original)
    debug_log("Step 2: Building hierarchical structure", 2)
    tree_structure <- list()

    for (domain in unique(domains)) {
      if (is.na(domain) || domain == "") next

      # Get terms for this domain
      domain_indices <- which(domains == domain)
      domain_terms <- descriptions[domain_indices]
      domain_go_ids <- go_ids[domain_indices]
      domain_pvals <- p_values[domain_indices]

      debug_log(paste("Building domain:", domain, "with", length(domain_terms), "terms"), 2)

      # Create intermediate groups for this domain (SAME function as original)
      intermediate_groups <- build_intermediate_groups_imported(
        terms = domain_terms,
        go_ids = domain_go_ids,
        p_values = domain_pvals,
        domain = domain,
        debug_log = debug_log
      )

      # Add to main tree structure
      tree_structure[[domain]] <- intermediate_groups
    }

    # Step 3: Format for shinyTree (SAME function as original)
    debug_log("Step 3: Formatting for shinyTree", 2)
    formatted_tree <- prepare_tree_for_shiny_imported(tree_structure, debug_log)

    debug_log(paste("Hierarchical tree completed -", length(formatted_tree), "top-level domains"), 1)
    return(formatted_tree)

  }, error = function(e) {
    debug_log(paste("Hierarchical tree failed:", e$message, "- using simple fallback"), 1)

    # Same fallback as original
    return(create_simple_tree_fallback_imported(imported_go_data, debug_log))
  })
}

#' Classify GO Domains - IDENTICAL to original function
#' @param go_ids vector of GO IDs
#' @param descriptions vector of GO descriptions
#' @param debug_log debug logging function
#' @return vector of domain names (identical to original)
classify_go_domains_imported <- function(go_ids, descriptions, debug_log) {

  domains <- character(length(go_ids))

  # Method 1: Try GO.db package for accurate mapping (same as original)
  tryCatch({
    if (require(GO.db, quietly = TRUE)) {
      debug_log("Using GO.db for accurate domain classification", 2)

      # Get ontology info
      ontology_data <- suppressMessages(AnnotationDbi::select(GO.db, keys = go_ids, columns = "ONTOLOGY", keytype = "GOID"))

      for (i in seq_along(go_ids)) {
        matching_rows <- which(ontology_data$GOID == go_ids[i])
        if (length(matching_rows) > 0) {
          ontology <- ontology_data$ONTOLOGY[matching_rows[1]]
          domains[i] <- switch(ontology,
                               "BP" = "Biological Process",
                               "MF" = "Molecular Function",
                               "CC" = "Cellular Component",
                               "Biological Process")  # Default
        } else {
          domains[i] <- "Biological Process"  # Default for unmapped
        }
      }

      mapped_count <- sum(!is.na(domains) & domains != "")
      debug_log(paste("GO.db successfully mapped", mapped_count, "of", length(domains), "terms"), 1)

      # If good mapping rate, return it
      if (mapped_count > length(domains) * 0.7) {
        return(domains)
      }
    }
  }, error = function(e) {
    debug_log(paste("GO.db classification failed:", e$message), 2)
  })

  # Method 2: Heuristic classification (IDENTICAL to original)
  debug_log("Using heuristic classification based on term content", 1)

  for (i in seq_along(descriptions)) {
    desc <- tolower(descriptions[i])

    # Cellular Component patterns (SAME as original)
    if (grepl("membrane|organelle|nucleus|nuclear|cytoplasm|cytosol|mitochondria|mitochondrial|ribosome|ribosomal|endoplasmic|golgi|vesicle|complex|assembly|compartment|envelope|chromosom|chromatin|cytoskeleton|vacuole|lysosome", desc)) {
      domains[i] <- "Cellular Component"
    }
    # Molecular Function patterns (SAME as original)
    else if (grepl("activity|binding|catalytic|kinase|phosphatase|transferase|hydrolase|ligase|lyase|isomerase|oxidoreductase|transporter|channel|receptor|enzyme|factor|inhibitor|activator", desc)) {
      domains[i] <- "Molecular Function"
    }
    # Biological Process patterns (SAME as original)
    else {
      domains[i] <- "Biological Process"
    }
  }

  # Log final distribution
  domain_counts <- table(domains)
  debug_log(paste("Domain distribution:", paste(names(domain_counts), "=", domain_counts, collapse = ", ")), 1)

  return(domains)
}

#' Build Intermediate Groups - IDENTICAL to original function
#' @param terms vector of GO term descriptions
#' @param go_ids vector of GO IDs
#' @param p_values vector of p-values for sorting
#' @param domain GO domain name
#' @param debug_log debug logging function
#' @return nested list of intermediate groups (identical to original)
build_intermediate_groups_imported <- function(terms, go_ids, p_values, domain, debug_log) {

  debug_log(paste("Building intermediate groups for", domain), 2)

  if (domain == "Biological Process") {
    return(group_biological_processes_imported(terms, go_ids, p_values, debug_log))
  } else if (domain == "Molecular Function") {
    return(group_molecular_functions_imported(terms, go_ids, p_values, debug_log))
  } else if (domain == "Cellular Component") {
    return(group_cellular_components_imported(terms, go_ids, p_values, debug_log))
  }

  # Fallback: alphabetical grouping (same as original)
  return(group_alphabetically_imported(terms, go_ids, p_values, debug_log))
}

#' Group Biological Process Terms - IDENTICAL category names and logic
group_biological_processes_imported <- function(terms, go_ids, p_values, debug_log) {

  groups <- list()

  # EXACT same categories and keywords as original
  bp_categories <- list(
    "Metabolic Processes" = c("metabolic", "metabolism", "biosynthetic", "biosynthesis", "catabolic", "catabolism", "synthesis", "degradation", "glycol", "lipid", "amino acid", "nucleotide"),

    "Gene Expression" = c("transcription", "translation", "gene expression", "protein synthesis", "mrna", "processing", "splicing"),

    "Cell Cycle & DNA" = c("cell cycle", "division", "mitosis", "meiosis", "replication", "dna repair", "chromosome", "mitotic", "meiotic", "s phase", "g1", "g2"),

    "Signaling" = c("signaling", "signal transduction", "pathway", "cascade", "communication", "detection", "recognition", "kinase cascade"),

    "Transport" = c("transport", "localization", "trafficking", "export", "import", "secretion", "endocytosis", "exocytosis"),

    "Development" = c("development", "developmental", "differentiation", "morphogenesis", "organogenesis", "embryonic", "pattern formation"),

    "Response Processes" = c("response", "stimulus", "stress", "defense", "immune", "inflammatory"),

    "Cell Death & Survival" = c("apoptosis", "cell death", "programmed cell death", "survival", "necrosis"),

    "Regulation" = c("regulation", "negative regulation", "positive regulation", "control", "inhibition", "activation"),

    "Other Processes" = c()  # Catch-all
  )

  # IDENTICAL classification logic
  for (i in seq_along(terms)) {
    term_lower <- tolower(terms[i])
    assigned <- FALSE

    # Check each category (same as original)
    for (category in names(bp_categories)) {
      if (category == "Other Processes") next  # Skip catch-all for now

      keywords <- bp_categories[[category]]
      if (any(sapply(keywords, function(kw) grepl(kw, term_lower, fixed = TRUE)))) {
        if (is.null(groups[[category]])) {
          groups[[category]] <- list()
        }
        groups[[category]][[terms[i]]] <- ""  # Empty string for shinyTree leaf
        assigned <- TRUE
        break
      }
    }

    # Add to catch-all if not assigned (same as original)
    if (!assigned) {
      if (is.null(groups[["Other Processes"]])) {
        groups[["Other Processes"]] <- list()
      }
      groups[["Other Processes"]][[terms[i]]] <- ""
    }
  }

  # Remove empty groups and sort by size (same as original)
  groups <- groups[sapply(groups, length) > 0]
  groups <- groups[order(sapply(groups, length), decreasing = TRUE)]

  return(groups)
}

#' Group Molecular Function Terms - IDENTICAL category names and logic
group_molecular_functions_imported <- function(terms, go_ids, p_values, debug_log) {

  groups <- list()

  # EXACT same categories as original
  mf_categories <- list(
    "Binding" = c("binding", "receptor binding", "protein binding", "dna binding", "rna binding", "ligand binding"),

    "Catalytic Activity" = c("activity", "catalytic", "enzyme", "catalysis", "hydrolase", "transferase", "ligase", "lyase", "isomerase", "oxidoreductase"),

    "Kinase Activity" = c("kinase", "phosphorylation", "phosphotransferase", "protein kinase"),

    "Phosphatase Activity" = c("phosphatase", "dephosphorylation", "phosphoprotein"),

    "Transcription Regulation" = c("transcription factor", "dna-binding", "sequence-specific", "regulatory", "promoter"),

    "Transport Function" = c("transporter", "channel", "permease", "carrier", "pump", "porter"),

    "Signal Transduction" = c("signal transducer", "receptor", "signaling receptor"),

    "Other Functions" = c()  # Catch-all
  )

  # IDENTICAL classification logic
  for (i in seq_along(terms)) {
    term_lower <- tolower(terms[i])
    assigned <- FALSE

    for (category in names(mf_categories)) {
      if (category == "Other Functions") next

      keywords <- mf_categories[[category]]
      if (any(sapply(keywords, function(kw) grepl(kw, term_lower, fixed = TRUE)))) {
        if (is.null(groups[[category]])) {
          groups[[category]] <- list()
        }
        groups[[category]][[terms[i]]] <- ""
        assigned <- TRUE
        break
      }
    }

    if (!assigned) {
      if (is.null(groups[["Other Functions"]])) {
        groups[["Other Functions"]] <- list()
      }
      groups[["Other Functions"]][[terms[i]]] <- ""
    }
  }

  groups <- groups[sapply(groups, length) > 0]
  groups <- groups[order(sapply(groups, length), decreasing = TRUE)]
  return(groups)
}

#' Group Cellular Component Terms - IDENTICAL category names and logic
group_cellular_components_imported <- function(terms, go_ids, p_values, debug_log) {

  groups <- list()

  # EXACT same categories as original
  cc_categories <- list(
    "Nuclear" = c("nucleus", "nuclear", "chromosome", "chromatin", "nucleoplasm", "nucleolus", "nuclear envelope"),

    "Membrane" = c("membrane", "plasma membrane", "cell membrane", "integral", "peripheral", "transmembrane", "lipid"),

    "Cytoplasm" = c("cytoplasm", "cytosol", "cytoplasmic", "cytoskeleton", "actin", "tubulin"),

    "Organelles" = c("mitochondria", "mitochondrial", "endoplasmic reticulum", "golgi", "lysosome", "peroxisome", "vacuole"),

    "Ribosomes" = c("ribosome", "ribosomal", "ribosomal subunit"),

    "Protein Complexes" = c("complex", "assembly", "machinery", "apparatus", "holoenzyme"),

    "Vesicles" = c("vesicle", "endosome", "transport vesicle", "secretory vesicle"),

    "Extracellular" = c("extracellular", "cell wall", "basement membrane", "matrix", "extracellular matrix"),

    "Other Components" = c()  # Catch-all
  )

  # IDENTICAL classification logic
  for (i in seq_along(terms)) {
    term_lower <- tolower(terms[i])
    assigned <- FALSE

    for (category in names(cc_categories)) {
      if (category == "Other Components") next

      keywords <- cc_categories[[category]]
      if (any(sapply(keywords, function(kw) grepl(kw, term_lower, fixed = TRUE)))) {
        if (is.null(groups[[category]])) {
          groups[[category]] <- list()
        }
        groups[[category]][[terms[i]]] <- ""
        assigned <- TRUE
        break
      }
    }

    if (!assigned) {
      if (is.null(groups[["Other Components"]])) {
        groups[["Other Components"]] <- list()
      }
      groups[["Other Components"]][[terms[i]]] <- ""
    }
  }

  groups <- groups[sapply(groups, length) > 0]
  groups <- groups[order(sapply(groups, length), decreasing = TRUE)]
  return(groups)
}

#' Group Alphabetically - IDENTICAL to original fallback
group_alphabetically_imported <- function(terms, go_ids, p_values, debug_log) {

  debug_log("Creating alphabetical groupings", 2)

  groups <- list()

  for (i in seq_along(terms)) {
    term <- terms[i]

    # Group by first letter (same as original)
    first_letter <- toupper(substr(trimws(term), 1, 1))
    if (!first_letter %in% LETTERS) {
      first_letter <- "Other"
    }

    group_name <- paste("Terms", first_letter)

    if (is.null(groups[[group_name]])) {
      groups[[group_name]] <- list()
    }
    groups[[group_name]][[term]] <- ""
  }

  # Sort alphabetically (same as original)
  groups <- groups[order(names(groups))]
  return(groups)
}

#' Prepare Tree for ShinyTree - IDENTICAL to original function
#' @param tree_structure nested list structure
#' @param debug_log debug logging function
#' @return properly formatted tree (identical to original)
prepare_tree_for_shiny_imported <- function(tree_structure, debug_log) {

  debug_log("Formatting tree for shinyTree widget", 2)

  # IDENTICAL recursive formatting as original
  format_node <- function(node) {
    if (is.list(node) && length(node) > 0) {
      # This is a branch node - format all children
      formatted_children <- list()
      for (name in names(node)) {
        formatted_children[[name]] <- format_node(node[[name]])
      }
      return(formatted_children)
    } else {
      # This is a leaf node - should be empty string
      return("")
    }
  }

  formatted_tree <- format_node(tree_structure)

  # Add shinyTree attributes for better UX (same as original)
  attr(formatted_tree, "stopened") <- TRUE  # Start with tree expanded

  debug_log("Tree formatting completed", 2)
  return(formatted_tree)
}

#' Simple Tree Fallback - IDENTICAL to original
#' @param imported_go_data imported GO results
#' @param debug_log debug logging function
#' @return simple flat tree structure (identical to original)
create_simple_tree_fallback_imported <- function(imported_go_data, debug_log) {

  debug_log("Creating simple fallback tree structure", 1)

  tryCatch({
    if (nrow(imported_go_data) == 0) {
      return(list("No terms found" = ""))
    }

    # Same logic as original simple tree
    imported_go_data <- imported_go_data[order(imported_go_data$p.adjust), ]
    max_terms <- min(50, nrow(imported_go_data))

    tree_list <- list()
    for (i in 1:max_terms) {
      term_name <- as.character(imported_go_data$Description[i])
      tree_list[[term_name]] <- ""
    }

    debug_log(paste("Fallback tree created with", length(tree_list), "terms"), 1)
    return(tree_list)

  }, error = function(e) {
    debug_log(paste("Even fallback failed:", e$message), 1)
    return(list("Error creating tree" = ""))
  })
}

#' Create proper GO results structure - FINAL VERSION WITH REAL S4
create_proper_go_results_structure <- function(imported_go_data, debug_log) {
  debug_log("=== CREATING PROPER RESULTS WITH REAL S4 OBJECT ===", 1)

  real_enrichresult <- create_real_enrichresult_object(imported_go_data, debug_log)

  if (is.null(real_enrichresult)) {
    debug_log("Failed to create real S4 object - using fallback", 1)
    real_enrichresult <- list(result = imported_go_data)
  }

  # Precompute pairwise similarity so Enrichment Map has edges
  edop <- tryCatch({
    if (requireNamespace("enrichplot", quietly = TRUE) &&
        inherits(real_enrichresult, "enrichResult")) {
      enrichplot::pairwise_termsim(real_enrichresult)
    } else {
      NULL
    }
  }, error = function(e) {
    debug_log(paste("pairwise_termsim failed:", e$message), 1)
    NULL
  })

  results_structure <- list(
    Edo_GO = real_enrichresult,
    Edop_GO = edop,
    FC_data = NULL,   # no FC available from Excel; plot code will simulate
    imported = TRUE
  )

  debug_log("=== REAL S4 RESULTS STRUCTURE CREATED ===", 1)
  return(results_structure)
}
