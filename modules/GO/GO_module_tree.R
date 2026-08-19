# ==============================================================================
# 8. Tree Structure Builders
# ==============================================================================

#' Create GO Tree Structure - HIERARCHICAL
#'
#' Creates 3-level hierarchical tree structure for GO term selection
#' @param edo enrichGO result object
#' @param debug_log logging function
#' @return nested list for shinyTree
create_go_tree_structure <- function(edo, debug_log = function(message, level = 1) {}) {

  tryCatch({
    debug_log("Creating hierarchical GO tree structure (3 levels)", 1)

    result_df <- edo@result
    if (nrow(result_df) == 0) {
      debug_log("No results found - returning empty tree", 1)
      return(list("No terms found" = ""))
    }

    result_df <- result_df[order(result_df$p.adjust), ]
    max_terms <- min(100, nrow(result_df))
    result_df <- result_df[1:max_terms, ]

    debug_log(paste("Processing", nrow(result_df), "GO terms for hierarchy"), 1)

    go_ids <- result_df$ID
    descriptions <- result_df$Description
    p_values <- result_df$p.adjust

    debug_log("Step 1: Classifying GO domains", 2)
    domains <- classify_go_domains(go_ids, descriptions, debug_log = debug_log)

    debug_log("Step 2: Building hierarchical structure", 2)
    tree_structure <- list()

    for (domain in unique(domains)) {
      if (is.na(domain) || domain == "") next

      domain_indices <- which(domains == domain)
      domain_terms <- descriptions[domain_indices]
      domain_go_ids <- go_ids[domain_indices]
      domain_pvals <- p_values[domain_indices]

      debug_log(paste("Building domain:", domain, "with", length(domain_terms), "terms"), 2)

      intermediate_groups <- build_intermediate_groups(
        terms = domain_terms,
        go_ids = domain_go_ids,
        p_values = domain_pvals,
        domain = domain,
        debug_log = debug_log
      )

      tree_structure[[domain]] <- intermediate_groups
    }

    debug_log("Step 3: Formatting for shinyTree", 2)
    formatted_tree <- prepare_tree_for_shiny(tree_structure, debug_log = debug_log)

    debug_log(paste("Hierarchical tree completed -", length(formatted_tree), "top-level domains"), 1)
    return(formatted_tree)

  }, error = function(e) {
    debug_log(paste("Hierarchical tree failed:", e$message, "- using simple fallback"), 1)
    return(create_simple_tree_fallback(edo, debug_log = debug_log))
  })
}

#' Classify GO Domains
#'
#' Determines GO domain (BP/MF/CC) for each term
#' @param go_ids vector of GO IDs
#' @param descriptions vector of GO descriptions
#' @param debug_log logging function
#' @return vector of domain names
classify_go_domains <- function(go_ids, descriptions, debug_log = function(message, level = 1) {}) {

  domains <- character(length(go_ids))

  tryCatch({
    if (require(GO.db, quietly = TRUE)) {
      debug_log("Using GO.db for accurate domain classification", 2)

      ontology_data <- suppressMessages(AnnotationDbi::select(GO.db, keys = go_ids, columns = "ONTOLOGY", keytype = "GOID"))

      for (i in seq_along(go_ids)) {
        matching_rows <- which(ontology_data$GOID == go_ids[i])
        if (length(matching_rows) > 0) {
          ontology <- ontology_data$ONTOLOGY[matching_rows[1]]
          domains[i] <- switch(ontology,
                               "BP" = "Biological Process",
                               "MF" = "Molecular Function",
                               "CC" = "Cellular Component",
                               "Biological Process")
        } else {
          domains[i] <- "Biological Process"
        }
      }

      mapped_count <- sum(!is.na(domains) & domains != "")
      debug_log(paste("GO.db successfully mapped", mapped_count, "of", length(domains), "terms"), 1)

      if (mapped_count > length(domains) * 0.7) {
        return(domains)
      }
    }
  }, error = function(e) {
    debug_log(paste("GO.db classification failed:", e$message), 2)
  })

  debug_log("Using heuristic classification based on term content", 1)

  for (i in seq_along(descriptions)) {
    desc <- tolower(descriptions[i])

    if (grepl("membrane|organelle|nucleus|nuclear|cytoplasm|cytosol|mitochondria|mitochondrial|ribosome|ribosomal|endoplasmic|golgi|vesicle|complex|assembly|compartment|envelope|chromosom|chromatin|cytoskeleton|vacuole|lysosome", desc)) {
      domains[i] <- "Cellular Component"
    } else if (grepl("activity|binding|catalytic|kinase|phosphatase|transferase|hydrolase|ligase|lyase|isomerase|oxidoreductase|transporter|channel|receptor|enzyme|factor|inhibitor|activator", desc)) {
      domains[i] <- "Molecular Function"
    } else {
      domains[i] <- "Biological Process"
    }
  }

  domain_counts <- table(domains)
  debug_log(paste("Domain distribution:", paste(names(domain_counts), "=", domain_counts, collapse = ", ")), 1)

  return(domains)
}

#' Build Intermediate Groups for Domain
#'
#' Creates intermediate category groupings within each GO domain
#' @param terms vector of GO term descriptions
#' @param go_ids vector of GO IDs
#' @param p_values vector of p-values for sorting
#' @param domain GO domain name
#' @param debug_log logging function
#' @return nested list of intermediate groups
build_intermediate_groups <- function(terms, go_ids, p_values, domain,
                                      debug_log = function(message, level = 1) {}) {

  debug_log(paste("Building intermediate groups for", domain), 2)

  if (domain == "Biological Process") {
    return(group_biological_processes(terms, go_ids, p_values, debug_log = debug_log))
  } else if (domain == "Molecular Function") {
    return(group_molecular_functions(terms, go_ids, p_values, debug_log = debug_log))
  } else if (domain == "Cellular Component") {
    return(group_cellular_components(terms, go_ids, p_values, debug_log = debug_log))
  }

  return(group_alphabetically(terms, go_ids, p_values, debug_log = debug_log))
}

#' Group Biological Process Terms
#' @param terms vector of GO term descriptions
#' @param go_ids vector of GO IDs
#' @param p_values vector of p-values
#' @param debug_log logging function
group_biological_processes <- function(terms, go_ids, p_values,
                                       debug_log = function(message, level = 1) {}) {

  groups <- list()

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

    "Other Processes" = c()
  )

  for (i in seq_along(terms)) {
    term_lower <- tolower(terms[i])
    assigned <- FALSE

    for (category in names(bp_categories)) {
      if (category == "Other Processes") next

      keywords <- bp_categories[[category]]
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
      if (is.null(groups[["Other Processes"]])) {
        groups[["Other Processes"]] <- list()
      }
      groups[["Other Processes"]][[terms[i]]] <- ""
    }
  }

  groups <- groups[sapply(groups, length) > 0]
  groups <- groups[order(sapply(groups, length), decreasing = TRUE)]

  return(groups)
}

#' Group Molecular Function Terms
#' @param terms vector of GO term descriptions
#' @param go_ids vector of GO IDs
#' @param p_values vector of p-values
#' @param debug_log logging function
group_molecular_functions <- function(terms, go_ids, p_values,
                                      debug_log = function(message, level = 1) {}) {

  groups <- list()

  mf_categories <- list(
    "Binding" = c("binding", "receptor binding", "protein binding", "dna binding", "rna binding", "ligand binding"),

    "Catalytic Activity" = c("activity", "catalytic", "enzyme", "catalysis", "hydrolase", "transferase", "ligase", "lyase", "isomerase", "oxidoreductase"),

    "Kinase Activity" = c("kinase", "phosphorylation", "phosphotransferase", "protein kinase"),

    "Phosphatase Activity" = c("phosphatase", "dephosphorylation", "phosphoprotein"),

    "Transcription Regulation" = c("transcription factor", "dna-binding", "sequence-specific", "regulatory", "promoter"),

    "Transport Function" = c("transporter", "channel", "permease", "carrier", "pump", "porter"),

    "Signal Transduction" = c("signal transducer", "receptor", "signaling receptor"),

    "Other Functions" = c()
  )

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

#' Group Cellular Component Terms
#' @param terms vector of GO term descriptions
#' @param go_ids vector of GO IDs
#' @param p_values vector of p-values
#' @param debug_log logging function
group_cellular_components <- function(terms, go_ids, p_values,
                                      debug_log = function(message, level = 1) {}) {

  groups <- list()

  cc_categories <- list(
    "Nuclear" = c("nucleus", "nuclear", "chromosome", "chromatin", "nucleoplasm", "nucleolus", "nuclear envelope"),

    "Membrane" = c("membrane", "plasma membrane", "cell membrane", "integral", "peripheral", "transmembrane", "lipid"),

    "Cytoplasm" = c("cytoplasm", "cytosol", "cytoplasmic", "cytoskeleton", "actin", "tubulin"),

    "Organelles" = c("mitochondria", "mitochondrial", "endoplasmic reticulum", "golgi", "lysosome", "peroxisome", "vacuole"),

    "Ribosomes" = c("ribosome", "ribosomal", "ribosomal subunit"),

    "Protein Complexes" = c("complex", "assembly", "machinery", "apparatus", "holoenzyme"),

    "Vesicles" = c("vesicle", "endosome", "transport vesicle", "secretory vesicle"),

    "Extracellular" = c("extracellular", "cell wall", "basement membrane", "matrix", "extracellular matrix"),

    "Other Components" = c()
  )

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

#' Group Alphabetically (Fallback)
#' @param terms vector of GO term descriptions
#' @param go_ids vector of GO IDs
#' @param p_values vector of p-values
#' @param debug_log logging function
group_alphabetically <- function(terms, go_ids, p_values,
                                 debug_log = function(message, level = 1) {}) {

  debug_log("Creating alphabetical groupings", 2)

  groups <- list()

  for (i in seq_along(terms)) {
    term <- terms[i]

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

  groups <- groups[order(names(groups))]
  return(groups)
}

#' Prepare Tree for ShinyTree
#'
#' Formats the tree structure properly for shinyTree widget
#' @param tree_structure nested list structure
#' @param debug_log logging function
#' @return properly formatted tree
prepare_tree_for_shiny <- function(tree_structure, debug_log = function(message, level = 1) {}) {

  debug_log("Formatting tree for shinyTree widget", 2)

  format_node <- function(node) {
    if (is.list(node) && length(node) > 0) {
      formatted_children <- list()
      for (name in names(node)) {
        formatted_children[[name]] <- format_node(node[[name]])
      }
      return(formatted_children)
    } else {
      return("")
    }
  }

  formatted_tree <- format_node(tree_structure)

  attr(formatted_tree, "stopened") <- TRUE

  debug_log("Tree formatting completed", 2)
  return(formatted_tree)
}

#' Simple Tree Fallback
#'
#' Creates original flat structure if hierarchy fails
#' @param edo enrichGO result
#' @param debug_log logging function
#' @return simple flat tree structure
create_simple_tree_fallback <- function(edo, debug_log = function(message, level = 1) {}) {

  debug_log("Creating simple fallback tree structure", 1)

  tryCatch({
    result_df <- edo@result
    if (nrow(result_df) == 0) {
      return(list("No terms found" = ""))
    }

    result_df <- result_df[order(result_df$p.adjust), ]
    max_terms <- min(50, nrow(result_df))

    tree_list <- list()
    for (i in 1:max_terms) {
      term_name <- as.character(result_df$Description[i])
      tree_list[[term_name]] <- ""
    }

    debug_log(paste("Fallback tree created with", length(tree_list), "terms"), 1)
    return(tree_list)

  }, error = function(e) {
    debug_log(paste("Even fallback failed:", e$message), 1)
    return(list("Error creating tree" = ""))
  })
}

# ==============================================================================
# 9. Term Selection Helpers
# ==============================================================================

#' Extract Selected Terms from Hierarchical Tree - Clean Debug Version
#'
#' Enhanced version that properly handles parent node selections
#' @param tree_input shinyTree input object
#' @param debug_log logging function
#' @return character vector of selected GO terms
extract_selected_terms_hierarchical <- function(tree_input, debug_log = function(message, level = 1) {}) {

  debug_log("Starting hierarchical term extraction", 2)

  selected_terms <- character(0)

  tryCatch({
    if (!is.list(tree_input) || length(tree_input) == 0) {
      debug_log("Tree input is empty or invalid", 1)
      return(selected_terms)
    }

    debug_log(paste("Processing tree with", length(tree_input), "domains"), 2)

    traverse_and_collect <- function(node, node_name = "", level = 1) {

      if (is.null(node)) return()

      debug_log(paste("Checking", node_name, "at level", level), 2)

      node_selected <- check_node_selection_enhanced(node, node_name, debug_log = debug_log)

      if (node_selected) {
        debug_log(paste("Selected:", node_name), 1)

        if (is.list(node) && length(node) > 0) {
          child_terms <- get_all_leaf_terms_enhanced(node, debug_log = debug_log)

          if (length(child_terms) > 0) {
            selected_terms <<- c(selected_terms, child_terms)
            debug_log(paste("Added", length(child_terms), "terms from", node_name), 1)
            debug_log(paste("Sample terms:", paste(head(child_terms, 3), collapse = ", ")), 2)
          } else {
            debug_log(paste("No child terms found for", node_name), 2)
          }
        } else {
          if (!is.null(node_name) && nchar(trimws(node_name)) > 0) {
            selected_terms <<- c(selected_terms, node_name)
            debug_log(paste("Added leaf term:", node_name), 2)
          }
        }

        return()
      }

      if (is.list(node) && length(node) > 0) {
        debug_log(paste("Traversing children of", node_name), 2)
        for (child_name in names(node)) {
          if (!is.null(child_name) && nchar(trimws(child_name)) > 0) {
            traverse_and_collect(node[[child_name]], child_name, level + 1)
          }
        }
      }
    }

    for (top_name in names(tree_input)) {
      if (!is.null(top_name) && nchar(trimws(top_name)) > 0) {
        debug_log(paste("Processing domain:", top_name), 2)
        traverse_and_collect(tree_input[[top_name]], top_name, 1)
      }
    }

    selected_terms <- unique(selected_terms)
    selected_terms <- selected_terms[!is.na(selected_terms) & nchar(trimws(selected_terms)) > 0]

    debug_log(paste("Hierarchical extraction completed:", length(selected_terms), "terms"), 1)
    if (length(selected_terms) > 0) {
      debug_log(paste("Sample selected:", paste(head(selected_terms, 3), collapse = ", ")), 2)
    }

    return(selected_terms)

  }, error = function(e) {
    debug_log(paste("Error in hierarchical extraction:", e$message), 1)
    return(selected_terms)
  })
}

#' Check Node Selection Status - Clean Debug Version
#'
#' Enhanced function to detect node selection with minimal debug output
#' @param node tree node object
#' @param name node name
#' @param debug_log logging function
#' @return logical indicating if node is selected
check_node_selection_enhanced <- function(node, name, debug_log = function(message, level = 1) {}) {

  is_selected <- FALSE
  selection_method <- "none"

  tryCatch({
    if (!is.null(attr(node, "stselected"))) {
      attr_value <- attr(node, "stselected")
      if (length(attr_value) == 1 && isTRUE(attr_value)) {
        is_selected <- TRUE
        selection_method <- "stselected"
      }
    }

    if (!is_selected && !is.null(attr(node, "selected"))) {
      attr_value <- attr(node, "selected")
      if (length(attr_value) == 1 && isTRUE(attr_value)) {
        is_selected <- TRUE
        selection_method <- "selected"
      }
    }

    if (!is_selected && is.list(node) && !is.null(node$selected)) {
      if (length(node$selected) == 1 && isTRUE(node$selected)) {
        is_selected <- TRUE
        selection_method <- "node$selected"
      }
    }

    if (!is_selected && !is.null(attr(node, "checked"))) {
      attr_value <- attr(node, "checked")
      if (length(attr_value) == 1 && isTRUE(attr_value)) {
        is_selected <- TRUE
        selection_method <- "checked"
      }
    }

    if (is_selected) {
      debug_log(paste(name, "-> selected via", selection_method), 2)
    }

  }, error = function(e) {
    debug_log(paste("Selection check error for", name, ":", e$message), 1)
  })

  return(is_selected)
}

#' Get All Leaf Terms from Tree Node - Clean Debug Version
#'
#' Recursively extracts all leaf terms (GO terms) from a hierarchical tree node
#' @param node tree node (list structure)
#' @param debug_log logging function
#' @return character vector of all leaf term names
get_all_leaf_terms_enhanced <- function(node, debug_log = function(message, level = 1) {}) {

  leaf_terms <- character(0)

  tryCatch({
    if (!is.list(node) || length(node) == 0) {
      debug_log("Node is not a list or is empty", 2)
      return(leaf_terms)
    }

    debug_log(paste("Processing node with", length(node), "children"), 2)

    for (name in names(node)) {
      if (is.null(name) || nchar(trimws(name)) == 0) next

      child <- node[[name]]

      is_leaf_node <- FALSE

      if (is.null(child)) {
        is_leaf_node <- TRUE
      } else if (length(child) == 1 && is.character(child) && nchar(trimws(child)) == 0) {
        is_leaf_node <- TRUE
      } else if (!is.list(child) && (is.character(child) || is.numeric(child) || is.logical(child))) {
        is_leaf_node <- TRUE
      }

      if (is_leaf_node) {
        leaf_terms <- c(leaf_terms, name)
        debug_log(paste("Found leaf term:", name), 2)
      } else if (is.list(child) && length(child) > 0) {
        debug_log(paste("Recursing into branch:", name), 2)
        child_leaves <- get_all_leaf_terms_enhanced(child, debug_log = debug_log)
        leaf_terms <- c(leaf_terms, child_leaves)
        debug_log(paste("Branch", name, "contributed", length(child_leaves), "terms"), 2)
      }
    }

    leaf_terms <- unique(leaf_terms)
    leaf_terms <- leaf_terms[!is.na(leaf_terms) & nchar(trimws(leaf_terms)) > 0]

    debug_log(paste("Extracted", length(leaf_terms), "leaf terms"), 1)
    return(leaf_terms)

  }, error = function(e) {
    debug_log(paste("Error extracting leaf terms:", e$message), 1)
    return(leaf_terms)
  })
}

#' Filter Terms by P-Value and Maximum Count - Clean Debug Version
#'
#' Filters a list of terms to keep only the most significant ones up to max count
#' @param terms character vector of GO term descriptions
#' @param go_results GO enrichment results object
#' @param max_terms maximum number of terms to return
#' @param debug_log logging function
#' @return character vector of filtered terms
filter_terms_by_pvalue <- function(terms, go_results, max_terms = 10,
                                   debug_log = function(message, level = 1) {}) {

  debug_log(paste("Filtering", length(terms), "terms to max", max_terms), 1)

  tryCatch({
    if (length(terms) <= max_terms) {
      debug_log("Terms already within limit", 2)
      return(terms)
    }

    if (is.list(go_results) && !is.null(go_results$Edo_GO)) {
      result_df <- go_results$Edo_GO@result
    } else if (inherits(go_results, "enrichResult")) {
      result_df <- go_results@result
    } else {
      debug_log("Cannot extract results - using first N terms", 2)
      return(head(terms, max_terms))
    }

    filtered_df <- result_df[result_df$Description %in% terms, ]

    if (nrow(filtered_df) == 0) {
      debug_log("No matching terms found in results", 1)
      return(character(0))
    }

    filtered_df <- filtered_df[order(filtered_df$p.adjust), ]
    selected_terms <- head(filtered_df$Description, max_terms)
    selected_terms <- as.character(selected_terms)

    debug_log(paste("Filtered to", length(selected_terms), "most significant terms"), 1)
    return(selected_terms)

  }, error = function(e) {
    debug_log(paste("Error in filtering:", e$message), 1)
    return(head(terms, max_terms))
  })
}
