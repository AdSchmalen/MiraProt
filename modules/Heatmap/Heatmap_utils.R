# Heatmap Module - Utility Functions
# Shared filtering, ordering, range, grid, object-validation, rendering, and
# alignment helpers. Statistical analysis is defined in the peer file
# Heatmap_statistical_analysis.R. Both files are sourced into the same module
# server lexical environment by Heatmap_module.R.

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# Normalize enrichment result containers without depending on clusterProfiler.
heatmap_normalize_enrichment_result <- function(value, prefer_first = FALSE) {
  if (is.null(value)) return(data.frame())
  candidates <- list(value)
  if (is.list(value) && !inherits(value, "data.frame")) {
    if ("Results" %in% names(value)) candidates <- c(list(value$Results), candidates)
    if (isTRUE(prefer_first) && length(value)) candidates <- c(list(value[[1]]), candidates)
  }
  for (candidate in candidates) {
    converted <- tryCatch(as.data.frame(candidate), error = function(e) NULL)
    if (inherits(converted, "data.frame")) return(converted)
  }
  data.frame()
}

heatmap_normalize_gsea_result <- function(value) heatmap_normalize_enrichment_result(value)
heatmap_normalize_go_result <- function(value) heatmap_normalize_enrichment_result(value, prefer_first = TRUE)

heatmap_pathway_choices <- function(result, id_fallback = FALSE) {
  if (!inherits(result, "data.frame") || !nrow(result)) return(character(0))
  column <- if ("Description" %in% names(result)) "Description" else if (isTRUE(id_fallback) && "ID" %in% names(result)) "ID" else NULL
  if (is.null(column)) return(character(0))
  values <- trimws(as.character(result[[column]]))
  unique(values[!is.na(values) & nzchar(values)])
}

heatmap_split_protein_fields <- function(fields) {
  if (is.null(fields)) return(character(0))
  values <- trimws(unlist(strsplit(as.character(fields), "[/;,\\r\\n]+")))
  unique(values[!is.na(values) & nzchar(values) & values != "NA"])
}

heatmap_extract_pathway_proteins <- function(result, pathways, type = c("gsea", "go")) {
  type <- match.arg(type)
  if (!inherits(result, "data.frame") || !nrow(result) || !length(pathways)) return(list())
  pathway_col <- if ("Description" %in% names(result)) "Description" else if (type == "gsea" && "ID" %in% names(result)) "ID" else NULL
  gene_col <- if (type == "gsea") {
    if ("core_enrichment" %in% names(result)) "core_enrichment" else NULL
  } else {
    intersect(c("geneID", "core_enrichment", "gene_id", "genes"), names(result))[1]
  }
  if (is.null(pathway_col) || is.null(gene_col) || is.na(gene_col)) return(list())
  resolved <- lapply(as.character(pathways), function(pathway) {
    rows <- !is.na(result[[pathway_col]]) & as.character(result[[pathway_col]]) == pathway
    heatmap_split_protein_fields(result[[gene_col]][rows])
  })
  resolved[lengths(resolved) > 0L]
}

heatmap_combine_pathway_proteins <- function(groups, intersecting = FALSE) {
  groups <- lapply(groups, heatmap_split_protein_fields)
  groups <- groups[lengths(groups) > 0L]
  if (!length(groups)) return(character(0))
  unique(if (isTRUE(intersecting)) Reduce(intersect, groups) else unlist(groups, use.names = FALSE))
}

heatmap_merge_identifier_proteins <- function(existing_text, proteins) {
  unique(c(heatmap_parse_identifier_text(existing_text), heatmap_split_protein_fields(proteins)))
}



resolve_heatmap_identifier_column <- function(identifier_value, data_def, data_mod) {
  identifier_value <- as.character(identifier_value %||% "")[1]
  if (is.na(identifier_value) || !nzchar(identifier_value) || !inherits(data_mod, "data.frame")) return(NULL)
  if (identifier_value %in% names(data_mod)) return(identifier_value)
  if (!inherits(data_def, "data.frame") || !all(c("Content", "Column") %in% names(data_def))) return(NULL)
  identifier_rows <- which(trimws(as.character(data_def$Content)) == "Identifier")
  if (length(identifier_rows) == 0L) return(NULL)
  identifier_columns <- as.character(data_def$Column[identifier_rows])
  option_values <- if ("Options" %in% names(data_def)) as.character(data_def$Options[identifier_rows]) else rep(NA_character_, length(identifier_rows))
  exact_option_match <- which(!is.na(option_values) & option_values == identifier_value & identifier_columns %in% names(data_mod))
  if (length(exact_option_match) > 0L) return(identifier_columns[exact_option_match[1]])
  exact_column_match <- which(identifier_columns == identifier_value & identifier_columns %in% names(data_mod))
  if (length(exact_column_match) > 0L) return(identifier_columns[exact_column_match[1]])
  NULL
}

validate_ratio_matrix <- function(ratio_matrix, context = "ratio_matrix") {
  if (is.null(ratio_matrix) || !is.matrix(ratio_matrix)) {
    heatmap_debug_log(paste(context, "invalid: expected matrix."), 1)
    return(NULL)
  }
  nr <- nrow(ratio_matrix); nc <- ncol(ratio_matrix)
  finite_count <- sum(is.finite(ratio_matrix), na.rm = TRUE)
  heatmap_debug_log(paste(context, "dims", nr, "x", nc, "finite", finite_count), 1)
  if (nr < 1 || nc < 1) {
    heatmap_debug_log(paste(context, "invalid: empty matrix dimensions."), 1)
    return(NULL)
  }
  ratio_matrix
}


# Extract and validate color scheme from UI input (3 colors: low/mid/high)
safe_gp <- function(size) {
  size_num <- suppressWarnings(as.numeric(size))
  grid::gpar(fontsize = max(6, size_num %||% 8))
}

# Create a finite matrix for distance-based clustering while preserving shape/labels.
# Used only to compute dendrogram geometry/order when NA values are present.
sanitize_matrix_for_clustering <- function(mat) {
  if (is.null(mat) || !is.matrix(mat)) return(mat)
  m <- suppressWarnings(matrix(as.numeric(mat), nrow = nrow(mat), ncol = ncol(mat), byrow = FALSE))
  rownames(m) <- rownames(mat)
  colnames(m) <- colnames(mat)
  m[!is.finite(m)] <- NA_real_
  if (!anyNA(m)) return(m)

  # Row-wise mean imputation (fallback 0 for all-NA rows).
  row_means <- rowMeans(m, na.rm = TRUE)
  row_means[!is.finite(row_means)] <- 0
  na_idx <- which(is.na(m), arr.ind = TRUE)
  if (nrow(na_idx) > 0) {
    m[na_idx] <- row_means[na_idx[, 1]]
  }
  m[!is.finite(m)] <- 0
  m
}


# Generic color function for arbitrary numeric values
# Sequential if non-negative; diverging symmetric around 0 otherwise.
col_fun_for_values <- function(values, palette, enhanced_contrast = FALSE) {
  vals <- as.numeric(values)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    heatmap_debug_log("Empty or non-finite values for color mapping; falling back to [-1, 1].", 2)
    vals <- c(-1, 0, 1)
  }
  vmin <- min(vals, na.rm = TRUE)
  vmax <- max(vals, na.rm = TRUE)
  if (vmin >= 0) {
    if (vmin == vmax) vmax <- vmin + 1
    range_stops <- seq(vmin, vmax, length.out = length(palette))
  } else {
    a <- max(abs(c(vmin, vmax)))
    range_stops <- seq(-a, a, length.out = length(palette))
  }

  if (!isTRUE(enhanced_contrast)) {
    return(circlize::colorRamp2(range_stops, palette))
  }

  heatmap_debug_log("Using enhanced contrast value color mapping", 1)
  linear_norm <- seq(-1, 1, length.out = 51)
  sigmoid_norm <- tanh(3.0 * linear_norm) / tanh(3.0)
  midpoint <- mean(range(range_stops))
  half_range <- diff(range(range_stops)) / 2
  enhanced_stops <- midpoint + half_range * sigmoid_norm
  pal_interp <- grDevices::colorRampPalette(palette)(length(enhanced_stops))
  circlize::colorRamp2(enhanced_stops, pal_interp)
}

# Single-column side heatmaps use only the low/high endpoint colors from the
# user palette. This keeps the neutral midpoint reserved for diverging
# expression/correlation heatmaps.
col_fun_for_single_column_values <- function(values, palette) {
  vals <- as.numeric(values)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    heatmap_debug_log("Empty or non-finite values for single-column color mapping; falling back to [0, 1].", 2)
    vals <- c(0, 1)
  }

  vmin <- min(vals, na.rm = TRUE)
  vmax <- max(vals, na.rm = TRUE)
  if (vmin == vmax) vmax <- vmin + 1

  endpoint_palette <- c(palette[1], palette[length(palette)])
  circlize::colorRamp2(c(vmin, vmax), endpoint_palette)
}

# Raw basemean-specific enhanced color function.
# Uses robust, non-symmetric quantile/IQR stops so one extreme raw abundance
# value saturates instead of flattening the contrast among the remaining proteins.
col_fun_for_raw_basemean <- function(values, palette, enhanced_contrast = FALSE) {
  if (!isTRUE(enhanced_contrast)) {
    return(col_fun_for_values(values, palette))
  }

  vals <- as.numeric(values)
  vals <- vals[is.finite(vals)]
  if (length(vals) < 3) {
    return(col_fun_for_values(vals, palette))
  }

  qs <- as.numeric(stats::quantile(
    vals,
    probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  ))

  if (length(qs) != 5 || any(!is.finite(qs)) || qs[1] == qs[5]) {
    return(col_fun_for_values(vals, palette))
  }

  iqr_val <- qs[4] - qs[2]
  lower_stop <- qs[1]
  upper_stop <- qs[5]
  if (is.finite(iqr_val) && iqr_val > 0) {
    lower_stop <- max(lower_stop, qs[2] - 1.5 * iqr_val)
    upper_stop <- min(upper_stop, qs[4] + 1.5 * iqr_val)
  }

  robust_stops <- c(lower_stop, qs[3], upper_stop)
  if (any(!is.finite(robust_stops)) || robust_stops[1] == robust_stops[3]) {
    return(col_fun_for_values(vals, palette))
  }

  if (robust_stops[2] <= robust_stops[1] || robust_stops[2] >= robust_stops[3]) {
    robust_stops[2] <- mean(robust_stops[c(1, 3)])
  }

  heatmap_debug_log(
    paste(
      "Using robust enhanced raw basemean color mapping with non-symmetric 5%/median/95%-IQR stops:",
      paste(signif(robust_stops, 4), collapse = ", ")
    ),
    1
  )

  circlize::colorRamp2(robust_stops, grDevices::colorRampPalette(palette)(3))
}

# Enhanced correlation-specific color function with sigmoid contrast enhancement
# Emphasizes middle colors and creates stronger contrast at extremes
col_fun_for_correlation <- function(palette, enhanced_contrast = FALSE) {
  if (!enhanced_contrast) {
    heatmap_debug_log("Using standard linear correlation color mapping", 2)
    return(circlize::colorRamp2(seq(-1, 1, length.out = length(palette)), palette))
  }

  heatmap_debug_log("Using sigmoid enhanced contrast correlation color mapping", 1)

  # Create more color stops for smoother gradients
  linear_stops <- seq(-1, 1, length.out = 51)  # 51 stops for smooth transitions

  # Apply sigmoid transformation to enhance contrast
  # This emphasizes the middle color and creates stronger contrast at extremes
  sigmoid_enhanced <- sapply(linear_stops, function(x) {
    # Sigmoid function: compresses middle values, expands extremes
    # Adjust steepness parameter (3.0) to control contrast enhancement
    steepness <- 3.0
    transformed <- tanh(steepness * x) / tanh(steepness)
    return(transformed)
  })

  # Ensure range stays exactly [-1, 1]
  sigmoid_enhanced <- pmax(-1, pmin(1, sigmoid_enhanced))

  # Create interpolated palette with enhanced stops
  pal_interp <- grDevices::colorRampPalette(palette)(length(sigmoid_enhanced))

  heatmap_debug_log(paste("Sigmoid enhanced contrast using", length(sigmoid_enhanced), "color stops"), 2)
  heatmap_debug_log(paste("Middle colors emphasized, extremes enhanced for better discrimination"), 1)

  return(circlize::colorRamp2(sigmoid_enhanced, pal_interp))
}

# Dendrogram helpers

rotate_dend_to_order <- function(dend, order_labels) {
  if (is.null(dend) || is.null(order_labels)) return(dend)
  order_labels <- as.character(order_labels)

  # Preferred path if dendextend is available.
  if (requireNamespace("dendextend", quietly = TRUE)) {
    out <- tryCatch({
      dendextend::rotate(dend, order_labels)
    }, error = function(e) {
      heatmap_debug_log(paste("Failed to rotate dendrogram via dendextend:", e$message), 2)
      NULL
    })
    if (!is.null(out)) return(out)
  }

  # Fallback without dendextend: reorder branches by target leaf positions.
  # This preserves dendrogram topology (real structure), only branch orientation changes.
  dend_labels <- tryCatch(as.character(labels(dend)), error = function(e) NULL)
  if (is.null(dend_labels)) {
    heatmap_debug_log("Dendrogram rotation fallback: labels unavailable; using unrotated dendrogram.", 2)
    return(dend)
  }

  w_map <- setNames(seq_along(order_labels), order_labels)
  if (!all(dend_labels %in% names(w_map))) {
    heatmap_debug_log("Dendrogram rotation fallback: label mismatch; using unrotated dendrogram.", 2)
    return(dend)
  }

  wts <- unname(w_map[dend_labels])
  out <- tryCatch(
    stats::reorder(dend, wts = wts, agglo.FUN = mean),
    error = function(e) {
      heatmap_debug_log(paste("Failed to rotate dendrogram via stats::reorder:", e$message), 2)
      NULL
    }
  )
  if (!is.null(out)) return(out)

  heatmap_debug_log("Dendrogram rotation fallback failed; using unrotated dendrogram.", 2)
  dend
}

ensure_dendrogram_order <- function(dend, order_labels, axis_label = "row", strict_order = TRUE) {
  if (is.null(order_labels) || is.null(dend)) return(NULL)
  order_labels <- as.character(order_labels)
  fallback_used <- FALSE
  fallback_reason <- "none"

  # Fallback that always respects the requested order.
  # Uses a chain-like hierarchy with monotone heights so it is always drawable.
  build_order_locked_fallback_dend <- function(labels, heights = NULL) {
    labels <- as.character(labels)
    n <- length(labels)
    if (n <= 1) {
      hcl <- list(
        merge = matrix(numeric(0), nrow = 0, ncol = 2),
        height = numeric(0),
        order = if (n == 1) 1L else integer(0),
        labels = labels,
        method = "order_locked_fallback",
        call = match.call(),
        dist.method = "none"
      )
      class(hcl) <- "hclust"
      return(stats::as.dendrogram(hcl))
    }

    merge <- matrix(0L, nrow = n - 1, ncol = 2)
    merge[1, ] <- c(-1L, -2L)
    if (n > 2) {
      for (i in 3:n) merge[i - 1, ] <- c(i - 2L, -i)
    }

    if (is.null(heights) || length(heights) != (n - 1)) {
      heights <- seq_len(n - 1)
    }
    heights <- as.numeric(heights)
    heights[!is.finite(heights)] <- 0
    eps <- .Machine$double.eps * 100
    heights <- cummax(heights + seq_along(heights) * eps)

    hcl <- list(
      merge = merge,
      height = heights,
      order = seq_len(n),
      labels = labels,
      method = "order_locked_fallback",
      call = match.call(),
      dist.method = "none"
    )
    class(hcl) <- "hclust"
    stats::as.dendrogram(hcl)
  }

  dend_labels <- tryCatch(as.character(labels(dend)), error = function(e) NULL)
  if (is.null(dend_labels)) {
    heatmap_debug_log(paste("INVARIANT:", axis_label, "dendrogram labels missing; using order-locked fallback."), 1)
    fallback_used <- TRUE
    fallback_reason <- "missing_labels"
    dend <- build_order_locked_fallback_dend(order_labels)
  } else {
    dend_label_set <- sort(dend_labels)
    order_label_set <- sort(order_labels)
    has_duplicate_labels <- anyDuplicated(order_labels) > 0

    if (!identical(dend_label_set, order_label_set)) {
      heatmap_debug_log(paste("INVARIANT VIOLATION:", axis_label, "dendrogram has different label set. Using order-locked fallback."), 1)
      heatmap_debug_log(paste("  Expected labels (first 5):", paste(head(order_label_set, 5), collapse=", ")), 1)
      heatmap_debug_log(paste("  Got labels      (first 5):", paste(head(dend_label_set, 5), collapse=", ")), 1)
      fallback_used <- TRUE
      fallback_reason <- "label_set_mismatch"
      dend <- build_order_locked_fallback_dend(order_labels)
    } else if (isTRUE(strict_order) && has_duplicate_labels) {
      # Duplicate labels make strict positional matching ambiguous.
      # In that case we keep the dendrogram (label set already validated)
      # and skip strict-order fallback to prevent repeated false violations.
      heatmap_debug_log(paste("INVARIANT:", axis_label, "strict order check skipped due to duplicate labels."), 2)
    } else if (isTRUE(strict_order) && !identical(unname(dend_labels), unname(order_labels))) {
      # Try one more deterministic rotation before falling back.
      dend_rot <- rotate_dend_to_order(dend, order_labels)
      dend_labels_rot <- tryCatch(as.character(labels(dend_rot)), error = function(e) NULL)

      if (!is.null(dend_labels_rot) && identical(unname(dend_labels_rot), unname(order_labels))) {
        dend <- dend_rot
      } else {
        mismatch_idx <- which(dend_labels != order_labels)
        first_mismatch <- if (length(mismatch_idx) > 0) mismatch_idx[1] else NA_integer_
        heatmap_debug_log(paste("INVARIANT VIOLATION:", axis_label, "dendrogram order differs in strict mode. Using order-locked fallback."), 1)
        heatmap_debug_log(paste("  First mismatch index:", ifelse(is.na(first_mismatch), "n/a", first_mismatch)), 1)
        heatmap_debug_log(paste("  Expected order (first 5):", paste(head(order_labels, 5), collapse=", ")), 1)
        heatmap_debug_log(paste("  Got order      (first 5):", paste(head(dend_labels, 5), collapse=", ")), 1)
        fallback_used <- TRUE
        fallback_reason <- "order_mismatch_strict"
        dend <- build_order_locked_fallback_dend(order_labels)
      }
    }
  }

  attr(dend, "fallback_used") <- fallback_used
  attr(dend, "fallback_reason") <- fallback_reason
  dend
}

# Single-source-of-truth for Pearson-r protein row ordering.
# Computes correlation matrix, hclust dendrogram (method="average"), and rotates
# to canonical (alphabetical) leaf order. Returns a named list with:
#   $order      -- character vector of protein names in leaf order
#   $dendrogram -- rotated dendrogram (ready for reindex_dendrogram_leaves)
#   $hclust     -- hclust object
# Returns NULL on any error.
compute_pearson_r_leaf_order <- function(expr_matrix) {
  tryCatch({
    if (is.null(expr_matrix) || nrow(expr_matrix) < 2) return(NULL)
    cor_matrix <- suppressWarnings(tryCatch({
      m <- stats::cor(t(expr_matrix), use = "pairwise.complete.obs", method = "pearson")
      m[is.na(m)] <- 0
      m
    }, error = function(e) NULL))
    if (is.null(cor_matrix)) return(NULL)
    rownames(cor_matrix) <- rownames(expr_matrix)
    colnames(cor_matrix) <- rownames(expr_matrix)

    d <- stats::as.dist(1 - cor_matrix)
    hc <- stats::hclust(d, method = "average")
    dend <- stats::as.dendrogram(hc)
    dend <- rotate_dend_to_order(dend, rownames(cor_matrix))

    leaf_order <- tryCatch(as.character(labels(dend)), error = function(e) character(0))
    if (length(leaf_order) != nrow(cor_matrix) || anyNA(leaf_order)) {
      leaf_order <- rownames(cor_matrix)[hc$order]
    }

    list(order = leaf_order, dendrogram = dend, hclust = hc)
  }, error = function(e) {
    heatmap_debug_log(paste("compute_pearson_r_leaf_order failed:", e$message), 1)
    NULL
  })
}

# Rewrite the integer leaf values of a dendrogram to 1, 2, 3, ..., n
# (left-to-right leaf visitation order) without changing tree structure,
# labels, heights, or any other attributes.
#
# Why this is needed: ComplexHeatmap calls order.dendrogram() internally and
# uses those integers to reorder the matrix rows/columns.  If the dendrogram
# was built from an already-reordered matrix the integers are relative to that
# matrix and will silently reorder the display again.  After reindexing,
# order.dendrogram(result) == seq_len(n), so ComplexHeatmap treats the
# dendrogram as purely decorative and preserves the existing row order.
reindex_dendrogram_leaves <- function(dend) {
  if (is.null(dend)) return(dend)
  counter_env <- new.env(parent = emptyenv())
  counter_env$count <- 0L

  reindex_node <- function(node) {
    if (is.leaf(node)) {
      counter_env$count <- counter_env$count + 1L
      old_attrs <- attributes(node)
      node <- counter_env$count
      attributes(node) <- old_attrs
    } else {
      for (i in seq_along(node)) {
        node[[i]] <- reindex_node(node[[i]])
      }
    }
    node
  }

  reindex_node(dend)
}

# Clustering for expression

build_order_locked_dendrogram <- function(mat, order_idx, axis = c("row", "column")) {
  axis <- match.arg(axis)
  n <- if (axis == "row") nrow(mat) else ncol(mat)
  if (n <= 1) {
    labels <- if (axis == "row") rownames(mat) else colnames(mat)
    hcl <- list(
      merge = matrix(numeric(0), nrow = 0, ncol = 2),
      height = numeric(0),
      order = if (n == 1) 1L else integer(0),
      labels = labels,
      method = "order_locked_ladder",
      call = match.call(),
      dist.method = "euclidean"
    )
    class(hcl) <- "hclust"
    return(stats::as.dendrogram(hcl))
  }

  ord <- as.integer(order_idx)
  ord <- ord[ord >= 1 & ord <= n]
  ord <- ord[!duplicated(ord)]
  if (length(ord) != n) ord <- seq_len(n)

  labels <- if (axis == "row") rownames(mat)[ord] else colnames(mat)[ord]

  # Deterministic chain heights for a stable, fixed-order "ladder" dendrogram.
  # This path is intentionally order-locked and should never imply data-driven clustering.
  heights <- seq_len(n - 1)

  merge <- matrix(0L, nrow = n - 1, ncol = 2)
  merge[1, ] <- c(-1L, -2L)
  if (n > 2) {
    for (i in 3:n) merge[i - 1, ] <- c(i - 2L, -i)
  }

  hcl <- list(
    merge = merge,
    height = heights,
    order = seq_len(n),
    labels = labels,
    method = "order_locked_ladder",
    call = match.call(),
    dist.method = "euclidean"
  )
  class(hcl) <- "hclust"
  stats::as.dendrogram(hcl)
}


compute_expression_clustering <- function(scaled_matrix, input) {
  tryCatch({
    clustering_matrix <- sanitize_matrix_for_clustering(scaled_matrix)

    row_dist <- stats::dist(clustering_matrix, method = "euclidean")
    row_hc <- stats::hclust(row_dist, method = "ward.D2")
    row_dend <- stats::as.dendrogram(row_hc)
    # Stabilize leaf orientation for tied distances / missing-value-heavy matrices.
    # Without this rotation, equivalent trees may flip left/right between runs.
    canonical_row_labels <- rownames(scaled_matrix)
    row_dend <- rotate_dend_to_order(row_dend, canonical_row_labels)
    row_order_idx <- match(as.character(labels(row_dend)), canonical_row_labels)
    if (any(is.na(row_order_idx)) || length(row_order_idx) != nrow(scaled_matrix)) {
      heatmap_debug_log("Row dendrogram stabilization failed; using original hclust order.", 1)
      row_order_idx <- stats::order.dendrogram(stats::as.dendrogram(row_hc))
    }

    col_order_idx <- seq_len(ncol(scaled_matrix))
    sample_sort_mode <- input$sort_samples_by %||% "none"

    if (identical(sample_sort_mode, "pearson_cluster")) {
      cor_mat <- suppressWarnings(stats::cor(clustering_matrix, use = "pairwise.complete.obs", method = "pearson"))
      cor_mat[is.na(cor_mat)] <- 0
      col_dist <- stats::as.dist(1 - cor_mat)
      col_hc <- stats::hclust(col_dist, method = "average")
      col_dend <- stats::as.dendrogram(col_hc)
      col_dend <- rotate_dend_to_order(col_dend, colnames(scaled_matrix))
    } else if (identical(sample_sort_mode, "distance_cluster")) {
      col_dist <- stats::dist(t(clustering_matrix), method = "euclidean")
      col_hc <- stats::hclust(col_dist, method = "ward.D2")
      col_dend <- stats::as.dendrogram(col_hc)
      col_dend <- rotate_dend_to_order(col_dend, colnames(scaled_matrix))
    } else {
      # Non-clustering sample orders keep user-chosen order and receive a constrained dendrogram.
      col_dend <- build_order_locked_dendrogram(scaled_matrix, col_order_idx, axis = "column")
    }

    list(
      row_order_idx = row_order_idx,
      col_order_idx = col_order_idx,
      row_dend = row_dend,
      col_dend = col_dend
    )
  }, error = function(e) {
    heatmap_debug_log(paste("Error computing expression clustering:", e$message), 1)
    list(
      row_order_idx = seq_len(nrow(scaled_matrix)),
      col_order_idx = seq_len(ncol(scaled_matrix)),
      row_dend = NULL,
      col_dend = NULL
    )
  })
}

# Expression heatmap builder

# Create the expression heatmap with clustering and UI-controlled dendrogram/labels visibility.
# Dendrograms are for display only; order is explicitly locked.
heatmap_rowwise_zscore <- function(log2_matrix) {
  z_mat <- t(apply(log2_matrix, 1, function(row_values) {
    row_mean <- mean(row_values, na.rm = TRUE)
    row_sd <- stats::sd(row_values, na.rm = TRUE)

    if (!is.finite(row_mean)) {
      return(rep(NA_real_, length(row_values)))
    }
    if (!is.finite(row_sd) || row_sd == 0) {
      z <- rep(0, length(row_values))
      z[is.na(row_values)] <- NA_real_
      return(z)
    }

    z <- (row_values - row_mean) / row_sd
    z[is.na(row_values)] <- NA_real_
    z
  }))

  # apply()/t() can drop column names; restore full dimnames explicitly so
  # downstream ordering/correlation logic keeps the selected sample order.
  dimnames(z_mat) <- dimnames(log2_matrix)
  z_mat
}

# Result processing and filtering

process_heatmap_results <- function(results, loadedData, data_def) {
  tryCatch({
    if (!"Identifier" %in% names(results)) {
      identifier_col <- which(grepl("Identifier|Gene|Protein", data_def$Options, ignore.case = TRUE))[1]
      if (!is.na(identifier_col) && identifier_col <= ncol(loadedData)) {
        if ("Row Index" %in% names(results)) {
          row_indices <- results$`Row Index`
          results$Identifier <- loadedData[[identifier_col]][row_indices]
        } else {
          results$Identifier <- loadedData[[identifier_col]][1:nrow(results)]
        }
      } else {
        if ("Row Index" %in% names(results)) {
          results$Identifier <- paste0("Protein_", results$`Row Index`)
        } else {
          results$Identifier <- paste0("Protein_", 1:nrow(results))
        }
      }
    }
    if (!"Row Index" %in% names(results)) {
      results$`Row Index` <- 1:nrow(results)
    }

    if (!"baseMean" %in% names(results)) {
      abundance_cols <- which(grepl("Abundance", data_def$Content, ignore.case = TRUE))
      if (length(abundance_cols) > 0 && "Row Index" %in% names(results)) {
        abundance_matrix <- as.matrix(loadedData[results$`Row Index`, abundance_cols])
        geom_mean_plus1 <- function(x) {
          valid_x <- x[!is.na(x) & x > 0]
          if (length(valid_x) == 0) return(0)
          exp(mean(log(valid_x + 1), na.rm = TRUE))
        }
        results$baseMean <- apply(abundance_matrix, 1, geom_mean_plus1)
      } else {
        results$baseMean <- 1
      }
    }

    pval_patterns <- c("p_Value", "p.value", "P.Value", "pvalue")
    adj_pval_patterns <- c("Adj_p_Value", "adj.p.value", "adj.P.Val", "p.adj", "FDR")

    for (pattern in pval_patterns) {
      matching_cols <- grep(pattern, names(results), ignore.case = TRUE, value = TRUE)
      if (length(matching_cols) > 0) {
        pval_col <- matching_cols[1]
        if (pval_col != "p.value") names(results)[names(results) == pval_col] <- "p.value"
        break
      }
    }
    for (pattern in adj_pval_patterns) {
      matching_cols <- grep(pattern, names(results), ignore.case = TRUE, value = TRUE)
      if (length(matching_cols) > 0) {
        adj_pval_col <- matching_cols[1]
        if (adj_pval_col != "adj.p.value") names(results)[names(results) == adj_pval_col] <- "adj.p.value"
        break
      }
    }
    if ("p.value" %in% names(results) && !"adj.p.value" %in% names(results)) {
      results$adj.p.value <- stats::p.adjust(results$p.value, method = "fdr")
    }
    results
  }, error = function(e) {
    heatmap_debug_log(paste("Error processing heatmap results:", e$message), 1)
    results
  })
}

filter_proteins_for_heatmap <- function(results, fdr_threshold = 0.05,
                                        basemean_threshold = 0, max_proteins = 50) {
  tryCatch({
    n_total <- nrow(results)
    if (n_total == 0) return(logical(0))

    fdr_col <- NULL
    fdr_patterns <- c("adj.p.value", "FDR", "adj.P.Val", "p.adj", "Adj_p_Value")
    for (pattern in fdr_patterns) {
      matching_cols <- grep(pattern, names(results), ignore.case = TRUE)
      if (length(matching_cols) > 0) { fdr_col <- matching_cols[1]; break }
    }
    if (is.null(fdr_col)) {
      pval_patterns <- c("p.value", "P.Value", "pvalue", "p_Value")
      for (pattern in pval_patterns) {
        matching_cols <- grep(pattern, names(results), ignore.case = TRUE)
        if (length(matching_cols) > 0) { fdr_col <- matching_cols[1]; break }
      }
    }

    if (is.null(fdr_col)) {
      selected <- rep(TRUE, n_total)
    } else {
      fdr_values <- results[[fdr_col]]
      selected <- !is.na(fdr_values) & fdr_values < fdr_threshold
    }

    if ("baseMean" %in% names(results)) {
      basemean_filter <- !is.na(results$baseMean) & results$baseMean >= basemean_threshold
      selected <- selected & basemean_filter
    }

    n_selected <- sum(selected, na.rm = TRUE)

    if (n_selected == 0) {
      if (!is.null(fdr_col)) {
        fdr_values <- results[[fdr_col]]
        valid_fdr <- !is.na(fdr_values)
        if (sum(valid_fdr) > 0) {
          n_to_select <- min(10, max_proteins, sum(valid_fdr))
          top_indices <- order(fdr_values, na.last = TRUE)[1:n_to_select]
          selected <- rep(FALSE, n_total); selected[top_indices] <- TRUE
        } else {
          n_to_select <- min(10, max_proteins, n_total)
          selected <- rep(FALSE, n_total); selected[1:n_to_select] <- TRUE
        }
      } else {
        n_to_select <- min(10, max_proteins, n_total)
        selected <- rep(FALSE, n_total); selected[1:n_to_select] <- TRUE
      }
    } else if (n_selected > max_proteins) {
      if (!is.null(fdr_col)) {
        fdr_values <- results[[fdr_col]]
        selected_indices <- which(selected)
        selected_fdr <- fdr_values[selected_indices]
        top_among_selected <- selected_indices[order(selected_fdr)[1:max_proteins]]
        selected <- rep(FALSE, n_total); selected[top_among_selected] <- TRUE
      } else {
        selected_indices <- which(selected)[1:max_proteins]
        selected <- rep(FALSE, n_total); selected[selected_indices] <- TRUE
      }
    }

    selected
  }, error = function(e) {
    heatmap_debug_log(paste("Error in protein filtering:", e$message), 1)
    n_total <- nrow(results)
    n_to_select <- min(10, n_total)
    selected <- rep(FALSE, n_total)
    if (n_to_select > 0) selected[1:n_to_select] <- TRUE
    selected
  })
}

# Annotation helpers (optional)

create_protein_annotation <- function(protein_names, highlight_proteins) {
  if (is.null(highlight_proteins) || length(highlight_proteins) == 0) return(NULL)
  highlight_indices <- which(protein_names %in% highlight_proteins)
  if (length(highlight_indices) == 0) return(NULL)

  ComplexHeatmap::rowAnnotation(
    Highlighted = ComplexHeatmap::anno_mark(
      at = highlight_indices,
      labels = protein_names[highlight_indices],
      labels_gp = grid::gpar(fontsize = 8),
      link_gp = grid::gpar(col = "red", lwd = 1)
    )
  )
}

# Heatmap extraction

extract_core_heatmap <- function(ht_object) {
  if (inherits(ht_object, "HeatmapList")) {
    heatmap_debug_log("Extracting core heatmap from HeatmapList", 2)
    ht_list <- ht_object@ht_list
    expr_idx <- which(sapply(ht_list, function(x)
      inherits(x, "Heatmap") && !is.null(x@name) && x@name == "Expression"))
    if (length(expr_idx) > 0) {
      heatmap_debug_log("Found Expression heatmap by name", 2)
      return(ht_list[[expr_idx[1]]])
    }
    matrix_idx <- which(sapply(ht_list, function(x)
      inherits(x, "Heatmap") && !is.null(x@matrix) && is.matrix(x@matrix)))
    if (length(matrix_idx) > 0) {
      heatmap_debug_log("Found Expression heatmap by matrix presence", 2)
      return(ht_list[[matrix_idx[1]]])
    }
    heatmap_debug_log("Using first heatmap as fallback", 2)
    return(ht_list[[1]])
  } else if (inherits(ht_object, "Heatmap")) {
    if (isTRUE(getOption("miraprot.heatmap.object_diagnostics", FALSE))) {
      heatmap_debug_log("Object is already a single Heatmap", 2)
    }
    return(ht_object)
  } else {
    stop("extract_core_heatmap: Object is neither Heatmap nor HeatmapList")
  }
}

# Alignment helper (optional)

align_expr_protein_by_row_labels <- function(
    expr_ht,
    protein_ht,
    stop_on_unmatched = TRUE,
    preserve_color_mapping = TRUE,
    keep_titles = TRUE
) {
  expr_ht_original <- expr_ht
  has_annotations <- inherits(expr_ht, "HeatmapList")
  if (has_annotations) {
    heatmap_debug_log("Expression heatmap is HeatmapList, extracting core heatmap", 2)
    expr_ht <- extract_core_heatmap(expr_ht)
  }
  stopifnot(inherits(expr_ht, "Heatmap"))
  stopifnot(inherits(protein_ht, "Heatmap"))

  `%or%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

  dx  <- ComplexHeatmap::draw(expr_ht)
  ord <- ComplexHeatmap::row_order(dx); if (is.list(ord)) ord <- unlist(ord, use.names = FALSE)

  expr_labels_all <- expr_ht@row_names_param$labels
  if (length(expr_labels_all) == 0 || all(is.na(expr_labels_all))) expr_labels_all <- rownames(expr_ht@matrix)
  expr_labels_all <- as.character(expr_labels_all)
  labx <- expr_labels_all[ord]
  ids  <- make.unique(labx)

  expr_mat <- expr_ht@matrix[ord, , drop = FALSE]
  rownames(expr_mat) <- ids

  dp   <- ComplexHeatmap::draw(protein_ht)
  pord_row <- ComplexHeatmap::row_order(dp); if (is.list(pord_row)) pord_row <- unlist(pord_row, use.names = FALSE)
  prot_labels_all <- protein_ht@row_names_param$labels
  if (length(prot_labels_all) == 0 || all(is.na(prot_labels_all))) prot_labels_all <- rownames(protein_ht@matrix)
  prot_labels_all <- as.character(prot_labels_all)
  labp <- prot_labels_all[pord_row]

  prot_mat <- protein_ht@matrix[pord_row, pord_row, drop = FALSE]

  if (isTRUE(stop_on_unmatched)) {
    idx <- match(ids, labp)
    if (anyNA(idx)) {
      missing_ids <- unique(ids[is.na(idx)])
      stop(sprintf(
        "align_expr_protein_by_row_labels: %d expression label(s) not found in protein labels, e.g.: %s",
        length(missing_ids), paste(utils::head(missing_ids, 10), collapse = ", ")
      ))
    }
    prot_mat <- prot_mat[idx, idx, drop = FALSE]
    dimnames(prot_mat) <- list(ids, ids)
  } else {
    common <- ids[ids %in% labp]
    if (length(common) == 0L) stop("align_expr_protein_by_row_labels: No common labels between Expression and Protein.")
    expr_mat <- expr_mat[match(common, ids), , drop = FALSE]
    common_idx_in_labp <- match(common, labp)
    prot_mat <- prot_mat[common_idx_in_labp, common_idx_in_labp, drop = FALSE]
    dimnames(prot_mat) <- list(common, common)
    ids <- common
  }

  expr_col_fun <- if (preserve_color_mapping) expr_ht@matrix_color_mapping@col_fun %or% NULL else NULL
  prot_col_fun <- if (preserve_color_mapping) protein_ht@matrix_color_mapping@col_fun %or% NULL else NULL

  expr_title <- if (keep_titles) expr_ht@column_title %or% NULL else NULL
  prot_title <- if (keep_titles) protein_ht@column_title %or% NULL else NULL

  expr_ht_fixed <- ComplexHeatmap::Heatmap(
    expr_mat,
    name = expr_ht@name %or% "Expression",
    col  = expr_col_fun,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    show_row_names = isTRUE(expr_ht@row_names_param$show),
    show_column_names = isTRUE(expr_ht@column_names_param$show),
    row_names_side = "left",
    column_title = expr_title
  )

  if (has_annotations) {
    heatmap_debug_log("Re-applying annotations to aligned expression heatmap", 2)
    tryCatch({
      if (inherits(expr_ht_original, "HeatmapList") && length(expr_ht_original@ht_list) > 1) {
        for (i in seq_along(expr_ht_original@ht_list)) {
          ht_item <- expr_ht_original@ht_list[[i]]
          if (inherits(ht_item, "HeatmapAnnotation")) {
            expr_ht_fixed <- ht_item + expr_ht_fixed
            heatmap_debug_log("Successfully re-applied identifier labels annotation", 2)
            break
          }
        }
      }
    }, error = function(e) {
      heatmap_debug_log(paste("Could not re-apply annotations:", e$message), 2)
    })
  }

  prot_ht_fixed <- ComplexHeatmap::Heatmap(
    prot_mat,
    name = protein_ht@name %or% "Protein Correlation",
    col  = prot_col_fun,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    show_row_names = isTRUE(protein_ht@row_names_param$show),
    show_column_names = isTRUE(protein_ht@row_names_param$show),
    row_names_side = "right",
    column_title = prot_title,
    row_order = seq_len(nrow(prot_mat)),
    column_order = seq_len(ncol(prot_mat)),
    row_names_gp = grid::gpar(fontsize = 8),
    column_names_gp = grid::gpar(fontsize = 8),
    column_names_rot = 90
  )

  heatmap_debug_log(paste("FINAL protein heatmap created with matrix dimensions:", paste(dim(prot_mat), collapse = "x")), 1)
  heatmap_debug_log(paste("Aligned protein correlation - row/col names identical:", identical(rownames(prot_mat), colnames(prot_mat))), 1)

  list(expr_ht_fixed = expr_ht_fixed, prot_ht_fixed = prot_ht_fixed, ids = ids)
}

# Sample Correlation Alignment (Columns)
# -------------------------

align_expr_sample_by_col_labels <- function(expr_ht, sample_ht) {
  tryCatch({
    heatmap_debug_log("Aligning expression and sample correlation heatmaps by column labels", 2)

    if (is.null(expr_ht) || is.null(sample_ht)) {
      heatmap_debug_log("Cannot align: one or both heatmaps are NULL", 1)
      return(NULL)
    }

    expr_core <- extract_core_heatmap(expr_ht)
    sample_core <- extract_core_heatmap(sample_ht)

    expr_mat <- expr_core@matrix
    sample_mat <- sample_core@matrix

    if (is.null(expr_mat) || is.null(sample_mat)) {
      heatmap_debug_log("Cannot align: one or both matrices are NULL", 1)
      return(NULL)
    }

    expr_col_names <- colnames(expr_mat)
    sample_row_names <- rownames(sample_mat)
    sample_col_names <- colnames(sample_mat)

    # Check if sample correlation has matching column names with expression
    common_names <- intersect(expr_col_names, sample_row_names)
    if (length(common_names) == 0) {
      heatmap_debug_log("No common names between expression columns and sample rows", 1)
      return(NULL)
    }

    # Ensure sample correlation matrix is square with same order
    if (!identical(sample_row_names, sample_col_names)) {
      heatmap_debug_log("Sample correlation matrix is not square - realigning", 2)
      sample_mat <- sample_mat[common_names, common_names, drop = FALSE]
    }

    # Create aligned matrices
    expr_aligned <- expr_mat[, common_names, drop = FALSE]
    sample_aligned <- sample_mat[common_names, common_names, drop = FALSE]

    # Rebuild expression heatmap (keep row order, align columns)
    expr_ht_fixed <- ComplexHeatmap::Heatmap(
      expr_aligned,
      name = expr_core@name,
      col = expr_core@matrix_color_mapping@col_fun,
      show_row_names = expr_core@row_names_param$show,
      show_column_names = expr_core@column_names_param$show,
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      row_order = seq_len(nrow(expr_aligned)),
      column_order = seq_len(ncol(expr_aligned))
    )

    # Rebuild sample correlation heatmap with aligned order
    sample_ht_fixed <- ComplexHeatmap::Heatmap(
      sample_aligned,
      name = sample_core@name,
      col = sample_core@matrix_color_mapping@col_fun,
      show_row_names = sample_core@row_names_param$show,
      show_column_names = sample_core@column_names_param$show,
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      row_order = seq_len(nrow(sample_aligned)),
      column_order = seq_len(ncol(sample_aligned))
    )

    heatmap_debug_log(paste("Expression heatmap aligned with matrix dimensions:", paste(dim(expr_aligned), collapse = "x")), 1)
    heatmap_debug_log(paste("Sample correlation aligned with matrix dimensions:", paste(dim(sample_aligned), collapse = "x")), 1)
    heatmap_debug_log(paste("Aligned sample correlation - row/col names identical:", identical(rownames(sample_aligned), colnames(sample_aligned))), 1)

    list(expr_ht_fixed = expr_ht_fixed, sample_ht_fixed = sample_ht_fixed, ids = common_names)
  }, error = function(e) {
    heatmap_debug_log(paste("Error in sample correlation alignment:", e$message), 1)
    NULL
  })
}

# -------------------------
# Extension heatmaps (UI-colored, correct labels)
# -------------------------

# Updated create_basemean_heatmap with strict alignment (NO CLUSTERING)
calculate_correct_basemean_internal <- function(selected_row_indices, loadedData, data_def, input) {
  tryCatch({
    skip_log_transform <- isTRUE(input$skip_log_transform_heatmap)
    basemean_scale <- if (skip_log_transform) "raw" else "log2"
    heatmap_debug_log(paste("Calculating", basemean_scale, "basemean values with metadata-driven approach"), 2)

    # NEW: Use select_samples_heatmap instead of numerator/denominator
    selected_abundance_type <- input$custom_col_sel_heatmap %||% "Normalized Abundance"
    selected_samples <- input$select_samples_heatmap %||% character(0)

    if (length(selected_samples) == 0) {
      heatmap_debug_log("No samples selected for basemean calculation", 2)
      return(NULL)
    }

    # Find columns matching the selected samples and abundance type
    matching_cols <- which(data_def$Content == selected_abundance_type &
                             data_def$Sample %in% selected_samples)

    if (length(matching_cols) == 0) {
      heatmap_debug_log("No matching columns found for basemean calculation", 2)
      return(NULL)
    }

    if (max(selected_row_indices) <= nrow(loadedData) && max(matching_cols) <= ncol(loadedData)) {
      basemean_data <- loadedData[selected_row_indices, matching_cols, drop = FALSE]

      # Apply re-transformation if needed
      tr_vec <- data_def$Transformation[matching_cols]
      if (any(!is.na(tr_vec) & tr_vec != "None" & tr_vec != "none")) {
        heatmap_debug_log("Re-transforming basemean data to original scale", 2)
        basemean_data <- retransform_data_global(
          basemean_data,
          seq_len(ncol(basemean_data)),
          tr_vec
        )
      }

      # Calculate arithmetic mean; optionally apply log2 to preserve the legacy display.
      basemean_values <- apply(basemean_data, 1, function(x) {
        if (skip_log_transform) {
          valid_x <- x[!is.na(x) & is.finite(x)]
          if (length(valid_x) == 0) {
            return(NA_real_)
          }
          return(mean(valid_x, na.rm = TRUE))
        }

        valid_x <- x[!is.na(x) & is.finite(x) & x > 0]
        if (length(valid_x) == 0) {
          return(0)  # Will be filtered out or set to minimum for log2
        }
        raw_mean <- mean(valid_x, na.rm = TRUE)
        # Apply log2 transformation, handle zero/negative values safely
        if (raw_mean <= 0) {
          return(0)  # Will be set to minimum value for visualization
        } else {
          return(log2(raw_mean))
        }
      })

      # Ensure finite values for visualization
      if (any(is.finite(basemean_values))) {
        min_val <- min(basemean_values[is.finite(basemean_values)], na.rm = TRUE)
        basemean_values[!is.finite(basemean_values)] <- min_val
      }

      heatmap_debug_log(paste("Calculated", basemean_scale, "basemean for", length(basemean_values), "proteins"), 2)
      return(basemean_values)
    } else {
      heatmap_debug_log("Index bounds exceeded in basemean calculation", 1)
      return(NULL)
    }
  }, error = function(e) {
    heatmap_debug_log(paste("Error calculating basemean:", e$message), 1)
    NULL
  })
}

# Updated calculate_abundance_ratios_internal (already implements log2, but ensure alignment)
calculate_abundance_ratios_internal <- function(selected_row_indices, loadedData, data_def, input) {
  tryCatch({
    heatmap_debug_log("Extracting log2 abundance ratios from metadata columns", 2)

    # NEW: Get the selected abundance ratio column directly from metadata
    ratio_col <- input$abundance_ratio_col_heatmap
    heatmap_debug_log(paste("Ratio-matrix generation contrast metadata: selected ratio column =", ratio_col %||% "<none>"), 1)

    if (is.null(ratio_col) || !nzchar(ratio_col)) {
      heatmap_debug_log("No abundance ratio column selected", 2)
      return(NULL)
    }

    # Find the column index
    ratio_idx <- which(data_def$Content == "Abundance Ratio" &
                         data_def$Column == ratio_col)
    heatmap_debug_log(paste("Ratio-matrix generation contrast metadata: matched index count =", length(ratio_idx)), 1)

    if (length(ratio_idx) != 1) {
      heatmap_debug_log("Could not find unique abundance ratio column", 1)
      return(NULL)
    }

    # Extract ratio values for selected rows
    if (max(selected_row_indices) <= nrow(loadedData)) {
      ratio_values <- loadedData[selected_row_indices, ratio_idx, drop = FALSE]
      heatmap_debug_log(paste("Ratio source subset dims:", nrow(ratio_values), "x", ncol(ratio_values)), 1)
      ratio_values <- ratio_values[, 1, drop = TRUE]

      # Convert to numeric
      ratio_values <- suppressWarnings(as.numeric(ratio_values))

      # Check for transformation and apply log2 if not already
      transform <- data_def$Transformation[ratio_idx]
      if (!is.na(transform) && transform == "log2") {
        # Already log2 transformed
        heatmap_debug_log("Abundance ratios already log2 transformed", 2)
      } else {
        # Apply log2 transformation
        heatmap_debug_log("Applying log2 transformation to abundance ratios", 2)
        # Handle zero/negative values
        ratio_values[ratio_values <= 0] <- NA
        ratio_values <- log2(ratio_values)
      }

      # Ensure finite values
      if (any(is.finite(ratio_values))) {
        min_val <- min(ratio_values[is.finite(ratio_values)], na.rm = TRUE)
        ratio_values[!is.finite(ratio_values)] <- min_val
      }

      heatmap_debug_log(paste("Extracted log2 abundance ratios for", length(ratio_values), "proteins; finite values =", sum(is.finite(ratio_values), na.rm = TRUE)), 1)
      return(ratio_values)
    } else {
      heatmap_debug_log("Index bounds exceeded in ratio extraction", 1)
      return(NULL)
    }
  }, error = function(e) {
    heatmap_debug_log(paste("Error extracting abundance ratios:", e$message), 1)
    NULL
  })
}

# ========================================
# Plot Extraction Helper Functions
# ========================================

# Extract plot object based on current tab
get_current_tab_plot_safe <- function(current_tab) {
  heatmap_debug_log(paste("Extracting plot for tab:", current_tab), 2)

  tryCatch({
    switch(current_tab,

           # Grid tabs - these need to be rendered fresh
           "grid_tab" = {
             plots <- heatmap_plots()
             if (is.null(plots) || is.null(plots$expr)) {
               return(list(status = "error", message = "No heatmap available. Please create heatmaps first."))
             }

             return(list(
               status = "success",
               type = "complex_combined",
               title = "Expression_Protein_Correlation_Grid",
               draw_function = function() {
                 # Use fixed objects for reliable download
                 fixed_expr <- heatmap_fixed_expression()
                 fixed_prot <- heatmap_fixed_protein_correlation()

                 if (!is.null(fixed_expr)) {
                   ht_list <- fixed_expr
                   if (!is.null(fixed_prot)) {
                     ht_list <- ht_list + fixed_prot
                   }

                   # Add basemean and ratio if available
                   fixed_basemean <- heatmap_fixed_basemean()
                   if (!is.null(fixed_basemean)) {
                     ht_list <- ht_list + fixed_basemean
                   }

                   fixed_ratio <- heatmap_fixed_abundance_ratio()
                   if (!is.null(fixed_ratio)) {
                     ht_list <- ht_list + fixed_ratio
                   }

                   # NEW: Legend side from UI
                   legend_side <- legend_side_from_input(input)
                   old_opt <- ComplexHeatmap::ht_opt()
                   on.exit({ try(ComplexHeatmap::ht_opt(old_opt), silent = TRUE) }, add = TRUE)
                   ComplexHeatmap::ht_opt(ROW_ANNO_PADDING = row_anno_padding_from_input(input))

                   ComplexHeatmap::draw(
                     ht_list,
                     newpage = FALSE,
                     merge_legends = TRUE,
                     heatmap_legend_side = legend_side,
                     annotation_legend_side = legend_side,
                     padding = heatmap_draw_padding_from_input(input)
                   )
                 } else {
                   stop("No fixed expression heatmap available")
                 }
               }
             ))
           },

           "expr_sample_tab" = {
             plots <- heatmap_plots()
             if (is.null(plots) || is.null(plots$expr)) {
               return(list(status = "error", message = "No heatmap available. Please create heatmaps first."))
             }

             return(list(
               status = "success",
               type = "complex_combined",
               title = "Expression_Sample_Correlation_Vertical",
               draw_function = function() {
                 fixed_expr <- heatmap_fixed_expression()
                 fixed_sample <- heatmap_fixed_sample_correlation()

                 if (!is.null(fixed_expr)) {
                   ht_list <- fixed_expr
                   if (!is.null(fixed_sample)) {
                     ht_list <- ht_list %v% fixed_sample  # Vertical layout
                   }

                   # NEW: Legend side from UI
                   legend_side <- legend_side_from_input(input)
                   old_opt <- ComplexHeatmap::ht_opt()
                   on.exit({ try(ComplexHeatmap::ht_opt(old_opt), silent = TRUE) }, add = TRUE)
                   ComplexHeatmap::ht_opt(ROW_ANNO_PADDING = row_anno_padding_from_input(input))

                   ComplexHeatmap::draw(
                     ht_list,
                     newpage = FALSE,
                     merge_legends = TRUE,
                     heatmap_legend_side = legend_side,
                     annotation_legend_side = legend_side,
                     padding = heatmap_draw_padding_from_input(input)
                   )
                 } else {
                   stop("No fixed expression heatmap available")
                 }
               }
             ))
           },

           # Individual tabs - use fixed objects
           "expression_tab" = {
             fixed_expr <- heatmap_fixed_expression()
             if (is.null(fixed_expr)) {
               return(list(status = "error", message = "No expression heatmap available. Create heatmap in grid tab first."))
             }
             return(list(status = "success", type = "complex_single", object = fixed_expr, title = "Expression_Heatmap"))
           },

           "protein_cor_tab" = {
             fixed_prot <- heatmap_fixed_protein_correlation()
             if (is.null(fixed_prot)) {
               return(list(status = "error", message = "No protein correlation heatmap available. Create heatmap in grid tab first."))
             }
             return(list(status = "success", type = "complex_single", object = fixed_prot, title = "Protein_Correlation_Heatmap"))
           },

           "sample_cor_tab" = {
             fixed_sample <- heatmap_fixed_sample_correlation()
             if (is.null(fixed_sample)) {
               return(list(status = "error", message = "No sample correlation heatmap available. Create heatmap in vertical tab first."))
             }
             return(list(status = "success", type = "complex_single", object = fixed_sample, title = "Sample_Correlation_Heatmap"))
           },

           "basemean_tab" = {
             fixed_basemean <- heatmap_fixed_basemean()
             if (is.null(fixed_basemean)) {
               return(list(status = "error", message = "No basemean heatmap available. Enable basemean in grid tab and create heatmaps first."))
             }
             return(list(status = "success", type = "complex_single", object = fixed_basemean, title = "Basemean_Heatmap"))
           },

           "abundance_ratio_tab" = {
             fixed_ratio <- heatmap_fixed_abundance_ratio()
             selected_ratio_col <- tryCatch(input$abundance_ratio_col_heatmap %||% "<none>", error = function(e) "<unavailable>")
             if (!is.null(fixed_ratio) && methods::is(fixed_ratio, "Heatmap")) {
               ratio_mat <- validate_ratio_matrix(fixed_ratio@matrix, context = "tab extraction abundance_ratio_tab")
               if (!is.null(ratio_mat)) {
                 heatmap_debug_log(paste("tab extraction abundance_ratio_tab metadata: selected ratio column =", selected_ratio_col), 1)
               }
             }
             if (is.null(fixed_ratio)) {
               return(list(status = "error", message = "No abundance ratio heatmap available. Enable abundance ratio in grid tab and create heatmaps first."))
             }
             return(list(status = "success", type = "complex_single", object = fixed_ratio, title = "Abundance_Ratio_Heatmap"))
           },

           # Debug tab or unknown
           {
             return(list(status = "error", message = paste("Cannot download from tab:", current_tab, ". Please switch to a heatmap tab.")))
           }
    )
  }, error = function(e) {
    heatmap_debug_log(paste("Error extracting plot for tab", current_tab, ":", e$message), 1)
    return(list(status = "error", message = paste("Error extracting plot:", e$message)))
  })
}





get_filter_string_Heatmap <- function(input_text, selected_identifier, debug_log) {
  debug_log("Parsing protein input text for Heatmap module", 2)

  lines <- unlist(strsplit(input_text, "\n"))
  lines <- trimws(lines[lines != ""])
  num_lines <- length(lines)

  if (num_lines == 0) {
    return(data.frame())
  }

  df <- data.frame(matrix(nrow = num_lines, ncol = 1))
  colnames(df) <- c(selected_identifier)

  for (i in 1:num_lines) {
    line <- unlist(strsplit(lines[i], "[,\\s]+"))
    df[i, selected_identifier] <- line[1]  # Takes first element if comma/space separated
  }

  debug_log(paste("Parsed", num_lines, "protein identifiers"), 2)
  return(df)
}

# Create protein annotation for highlighting
create_protein_annotation <- function(expr_row_names, highlighted_proteins, font_size = 8, label_padding_mm = 1) {
  tryCatch({
    if (is.null(highlighted_proteins) || length(highlighted_proteins) == 0) {
      return(NULL)
    }

    # Find which highlighted proteins are actually in the heatmap
    proteins_in_heatmap <- intersect(highlighted_proteins, expr_row_names)

    if (length(proteins_in_heatmap) == 0) {
      heatmap_debug_log("No highlighted proteins found in current heatmap", 1)
      return(NULL)
    }

    # Get indices for annotation
    protein_indices <- which(expr_row_names %in% proteins_in_heatmap)
    protein_labels <- expr_row_names[protein_indices]

    heatmap_debug_log(paste("Creating annotation for", length(protein_indices), "proteins"), 1)
    heatmap_debug_log(paste("Protein indices:", paste(head(protein_indices, 5), collapse = ", ")), 2)

    # Create rowAnnotation with anno_mark (positioned on the right)
    label_padding_mm <- suppressWarnings(as.numeric(label_padding_mm))
    if (!is.finite(label_padding_mm)) label_padding_mm <- 1
    label_padding_mm <- max(0, min(label_padding_mm, 40))

    annotation <- ComplexHeatmap::rowAnnotation(
      highlighted = ComplexHeatmap::anno_mark(
        at = protein_indices,
        labels = protein_labels,
        labels_gp = grid::gpar(fontsize = font_size),
        padding = grid::unit(label_padding_mm, "mm"),
        link_width = grid::unit(8, "mm"),  # Slightly longer links for better visibility
        side = "right"  # Explicit right positioning
      ),
      width = grid::unit(20 + label_padding_mm, "mm"),  # Wider annotation canvas for dense highlight labels
      show_annotation_name = FALSE  # Hide "highlighted" label
    )

    return(annotation)

  }, error = function(e) {
    heatmap_debug_log(paste("Error creating protein annotation:", e$message), 1)
    return(NULL)
  })
}

# ========================================
# SEPARATE FILTERING FUNCTIONS
# ========================================

#' @param pval_threshold p-value threshold
#' @return updated logical mask
heatmap_apply_pvalue_filter <- function(dm, dd, current_mask, pval_type, pval_col, pval_threshold) {
  tryCatch({
    if (is.null(pval_col) || !nzchar(pval_col)) {
      heatmap_debug_log("No p-value column specified - skipping p-value filter", 2)
      return(current_mask)
    }

    # Find p-value column index
    p_idx <- which(dd$Content == pval_type & dd$Column == pval_col)

    if (length(p_idx) == 0) {
      heatmap_debug_log("P-value column not found in data definition - skipping p-value filter", 1)
      return(current_mask)
    }

    if (length(p_idx) > 1) {
      heatmap_debug_log("Multiple p-value columns found - using first", 1)
      p_idx <- p_idx[1]
    }

    # Extract p-values
    p_vec <- suppressWarnings(as.numeric(dm[[p_idx]]))

    # Apply p-value threshold
    valid_pvals <- !is.na(p_vec) & is.finite(p_vec) & p_vec >= 0 & p_vec <= 1
    passing_pvals <- valid_pvals & p_vec <= pval_threshold

    # Update mask: keep current selection AND p-value criterion
    updated_mask <- current_mask & passing_pvals

    heatmap_debug_log(paste("P-value filter applied:", sum(passing_pvals), "proteins passed threshold", pval_threshold), 2)
    return(updated_mask)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in p-value filtering:", e$message), 1)
    return(current_mask)
  })
}

#' Apply abundance ratio filtering step
#' @param dm data matrix
#' @param dd data definition
#' @param current_mask logical vector of currently selected proteins
#' @param ratio_col ratio column name
#' @param filter_mode filter mode
#' @param threshold threshold value
#' @return updated logical mask
heatmap_apply_ratio_filter <- function(dm, dd, current_mask, ratio_col, filter_mode, threshold, rv = NULL) {
  tryCatch({
    # Check ratio availability
    ratio_check <- heatmap_check_ratio_availability_enhanced(rv)

    if (!ratio_check$available) {
      heatmap_debug_log(paste("Ratio filtering requested but not available:", ratio_check$reason), 1)
      return(current_mask)
    }

    if (is.null(ratio_col) || !nzchar(ratio_col)) {
      heatmap_debug_log("No ratio column specified - skipping ratio filter", 2)
      return(current_mask)
    }

    # Find ratio column index
    r_idx <- which(dd$Content == "Abundance Ratio" & dd$Column == ratio_col)

    if (length(r_idx) == 0) {
      heatmap_debug_log("Ratio column not found in data definition - skipping ratio filter", 1)
      return(current_mask)
    }

    if (length(r_idx) > 1) {
      heatmap_debug_log("Multiple ratio columns found - using first", 1)
      r_idx <- r_idx[1]
    }

    # Extract ratio values
    r_vec <- suppressWarnings(as.numeric(dm[[r_idx]]))

    # Apply ratio filter using existing enhanced function
    r_pass <- heatmap_apply_log2_ratio_filter(r_vec, filter_mode, threshold)

    # Update mask: keep current selection AND ratio criterion
    updated_mask <- current_mask & r_pass

    heatmap_debug_log(paste("Ratio filter applied:", sum(r_pass), "proteins passed filter"), 2)
    return(updated_mask)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in ratio filtering:", e$message), 1)
    return(current_mask)
  })
}


#' Parse comma- or line-separated protein identifiers.
#' Mirrors the Identifier Filter parsing behavior.
heatmap_parse_identifier_text <- function(input_text) {
  if (is.null(input_text)) return(character(0))
  input_text <- paste(as.character(input_text), collapse = "\n")
  if (!nzchar(input_text)) return(character(0))
  parsed <- trimws(unlist(strsplit(input_text, "[,\n]+")))
  unique(parsed[nzchar(parsed)])
}

#' Summarize identifier filter parsing and exact row matches.
#'
#' The parsed identifiers intentionally come from heatmap_parse_identifier_text(),
#' the same parser used by heatmap_apply_identifier_filter(), so diagnostics and
#' filtering stay aligned without changing matching behavior.
heatmap_identifier_filter_details <- function(custom_filter_text, identifiers_chr) {
  raw_text <- paste(as.character(custom_filter_text %||% ""), collapse = "\n")
  raw_entries <- if (nzchar(raw_text)) {
    raw <- trimws(unlist(strsplit(raw_text, "[,\n]+")))
    raw[nzchar(raw)]
  } else {
    character(0)
  }

  parsed_identifiers <- heatmap_parse_identifier_text(raw_text)
  identifiers_chr <- trimws(as.character(identifiers_chr %||% character(0)))
  match_mask <- identifiers_chr %in% parsed_identifiers
  match_mask[is.na(match_mask)] <- FALSE
  matched_identifiers <- identifiers_chr[match_mask]
  matched_identifiers <- matched_identifiers[!is.na(matched_identifiers) & nzchar(matched_identifiers)]

  list(
    raw_entries = raw_entries,
    parsed_identifiers = parsed_identifiers,
    match_mask = match_mask,
    matched_rows = sum(match_mask),
    matched_identifiers = matched_identifiers,
    unique_matched_identifiers = unique(matched_identifiers)
  )
}

#' Apply custom priority ordering to a fallback protein order.
#' Matching identifiers are placed first, duplicate custom entries keep their
#' first occurrence, and all remaining proteins keep fallback order.
heatmap_apply_custom_protein_order <- function(fallback_order, custom_order_text) {
  fallback_order <- as.character(fallback_order %||% character(0))
  if (length(fallback_order) == 0) return(fallback_order)

  custom_proteins <- heatmap_parse_identifier_text(custom_order_text)
  if (length(custom_proteins) == 0) return(fallback_order)

  custom_proteins <- custom_proteins[!duplicated(custom_proteins)]
  matched_custom <- custom_proteins[custom_proteins %in% fallback_order]
  if (length(matched_custom) == 0) return(fallback_order)

  c(matched_custom, fallback_order[!(fallback_order %in% matched_custom)])
}

#' Resolve the fallback protein order used after custom priority matches.
heatmap_custom_fallback_order <- function(expr_matrix, input) {
  if (is.null(expr_matrix) || nrow(expr_matrix) == 0) return(character(0))

  fallback_sort <- input$custom_protein_fallback_sort %||% "z_score"
  if (identical(fallback_sort, "pearson_r")) {
    pearson_result <- compute_pearson_r_leaf_order(expr_matrix)
    if (!is.null(pearson_result) && length(pearson_result$order) > 0) {
      return(intersect(pearson_result$order, rownames(expr_matrix)))
    }
  }

  cl_info <- compute_expression_clustering(expr_matrix, input)
  if (!is.null(cl_info$row_order_idx) && length(cl_info$row_order_idx) == nrow(expr_matrix)) {
    return(rownames(expr_matrix)[cl_info$row_order_idx])
  }

  rownames(expr_matrix)
}

#' Apply identifier filtering step
#' @param current_mask logical vector of currently selected proteins
#' @param identifiers_chr character vector of protein identifiers
#' @param custom_filter_text text input with custom protein identifiers
#' @return updated logical mask
heatmap_apply_identifier_filter <- function(current_mask, identifiers_chr, custom_filter_text) {
  tryCatch({
    if (!nzchar(custom_filter_text)) {
      heatmap_debug_log("No custom identifiers specified - skipping identifier filter", 2)
      return(current_mask)
    }

    # Parse custom proteins from text input
    custom_proteins <- heatmap_parse_identifier_text(custom_filter_text)

    if (length(custom_proteins) == 0) {
      heatmap_debug_log("No valid custom proteins found after parsing - skipping identifier filter", 2)
      return(current_mask)
    }

    # Create identifier mask
    custom_mask <- identifiers_chr %in% custom_proteins

    # Update mask: keep current selection AND identifier criterion
    updated_mask <- current_mask & custom_mask

    heatmap_debug_log(paste("Identifier filter applied:", sum(custom_mask), "proteins matched from", length(custom_proteins), "specified"), 2)
    return(updated_mask)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in identifier filtering:", e$message), 1)
    return(current_mask)
  })
}

#' Apply max protein filtering step (final step)
#' @param dm data matrix
#' @param dd data definition
#' @param current_mask logical vector of currently selected proteins
#' @param max_proteins maximum number of proteins to select
#' @param pval_type p-value type (for ranking if available)
#' @param pval_col p-value column (for ranking if available)
#' @return updated logical mask
heatmap_apply_max_protein_filter <- function(dm, dd, current_mask, max_proteins, pval_type = NULL, pval_col = NULL) {
  tryCatch({
    current_selected <- sum(current_mask)

    # If already within limit, no filtering needed
    if (current_selected <= max_proteins) {
      heatmap_debug_log(paste("Current selection", current_selected, "within max proteins limit", max_proteins, "- no max filtering needed"), 2)
      return(current_mask)
    }

    heatmap_debug_log(paste("Applying max protein filter: reducing from", current_selected, "to", max_proteins, "proteins"), 1)

    # Get indices of currently selected proteins
    selected_indices <- which(current_mask)

    # Try to use p-values for intelligent ranking if available
    use_pvalue_ranking <- !is.null(pval_col) && nzchar(pval_col)

    if (use_pvalue_ranking) {
      # Find p-value column and use for ranking
      p_idx <- which(dd$Content == pval_type & dd$Column == pval_col)

      if (length(p_idx) == 1) {
        p_vec <- suppressWarnings(as.numeric(dm[[p_idx]]))
        selected_pvals <- p_vec[selected_indices]

        # Check if we have valid p-values for ranking
        valid_pvals <- !is.na(selected_pvals) & is.finite(selected_pvals)

        if (sum(valid_pvals) >= max_proteins) {
          # Rank by p-value (ascending - smallest p-values first)
          pval_order <- order(selected_pvals, na.last = TRUE)
          top_indices <- selected_indices[pval_order[1:max_proteins]]

          # Create new mask
          new_mask <- rep(FALSE, length(current_mask))
          new_mask[top_indices] <- TRUE

          heatmap_debug_log(paste("Max protein filter applied using p-value ranking:", max_proteins, "proteins selected"), 1)
          return(new_mask)
        } else {
          heatmap_debug_log("Insufficient valid p-values for ranking - using random selection", 2)
        }
      } else {
        heatmap_debug_log("P-value column not found for ranking - using random selection", 2)
      }
    }

    # Fallback: random selection
    if (current_selected > max_proteins) {
      random_indices <- sample(selected_indices, max_proteins, replace = FALSE)

      # Create new mask
      new_mask <- rep(FALSE, length(current_mask))
      new_mask[random_indices] <- TRUE

      heatmap_debug_log(paste("Max protein filter applied using random selection:", max_proteins, "proteins selected"), 1)
      return(new_mask)
    }

    # Should not reach here, but return original mask as fallback
    return(current_mask)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in max protein filtering:", e$message), 1)

    # Emergency fallback: take first N proteins
    selected_indices <- which(current_mask)
    emergency_indices <- selected_indices[1:min(max_proteins, length(selected_indices))]
    emergency_mask <- rep(FALSE, length(current_mask))
    emergency_mask[emergency_indices] <- TRUE

    heatmap_debug_log("Applied emergency max protein filter using first N proteins", 1)
    return(emergency_mask)
  })
}

#' Enhanced protein selection with improved p-value column validation
#' @param dm data matrix
#' @param dd data definition
#' @param enable_pvalue_filter logical, whether p-value filtering is enabled
#' @param pval_type p-value type (if available)
#' @param pval_col p-value column (if available)
#' @param pval_threshold p-value threshold
#' @return logical vector indicating selected proteins
heatmap_robust_protein_selection <- function(dm, dd, enable_pvalue_filter, pval_type, pval_col, pval_threshold) {
  tryCatch({
    total_proteins <- nrow(dm)
    heatmap_debug_log(paste("Starting robust protein selection from", total_proteins, "total proteins"), 1)

    # Initialize mask - all proteins are candidates initially
    mask <- rep(TRUE, total_proteins)

    # Apply p-value filtering if enabled and available
    if (enable_pvalue_filter && !is.null(pval_type) && !is.null(pval_col) &&
        nzchar(pval_type) && nzchar(pval_col)) {

      heatmap_debug_log(paste("Applying p-value filtering with threshold <=", pval_threshold), 2)
      heatmap_debug_log(paste("Looking for Content =", pval_type, "AND Column =", pval_col), 2)

      # Find p-value column index - ENHANCED validation
      p_idx <- which(dd$Content == pval_type & dd$Column == pval_col)

      if (length(p_idx) == 1) {
        p_vec <- suppressWarnings(as.numeric(dm[[p_idx]]))

        # Validate p-values are in reasonable range
        valid_p_values <- p_vec[is.finite(p_vec)]
        if (length(valid_p_values) == 0) {
          heatmap_debug_log("No valid p-values found in selected column", 1)
          mask <- rep(FALSE, total_proteins)
        } else if (any(valid_p_values < 0 | valid_p_values > 1)) {
          heatmap_debug_log("Warning: Some p-values outside 0-1 range", 1)
        }

        # Apply p-value threshold
        if (any(is.finite(p_vec))) {
          p_pass <- p_vec <= pval_threshold & is.finite(p_vec)
          p_pass[is.na(p_pass)] <- FALSE
          mask <- mask & p_pass

          heatmap_debug_log(paste("P-value filter:", sum(p_pass), "proteins pass out of",
                          sum(is.finite(p_vec)), "with valid p-values"), 2)

          # Show p-value range of passing proteins for debugging
          if (sum(p_pass) > 0) {
            passing_pvals <- p_vec[p_pass]
            heatmap_debug_log(paste("Passing p-value range:",
                            round(min(passing_pvals), 6), "to",
                            round(max(passing_pvals), 6)), 1)
          }
        } else {
          heatmap_debug_log("No finite p-values found, p-value filter failed", 1)
          mask <- rep(FALSE, total_proteins)
        }
      } else if (length(p_idx) == 0) {
        heatmap_debug_log(paste("P-value column not found. Content =", pval_type, ", Column =", pval_col), 1)
        heatmap_debug_log(paste("Available combinations:"), 1)
        if ("Column" %in% names(dd)) {
          available_combos <- unique(paste(dd$Content, dd$Column, sep = " / "))
          heatmap_debug_log(paste("  ", paste(head(available_combos, 10), collapse = ", ")), 1)
        }
        mask <- rep(FALSE, total_proteins)
      } else {
        heatmap_debug_log(paste("Multiple p-value columns found (", length(p_idx), "), using first"), 1)
        p_vec <- suppressWarnings(as.numeric(dm[[p_idx[1]]]))
        p_pass <- p_vec <= pval_threshold & is.finite(p_vec)
        p_pass[is.na(p_pass)] <- FALSE
        mask <- mask & p_pass
      }
    } else {
      heatmap_debug_log("P-value filtering disabled or parameters not available", 1)
    }

    # Count proteins passing filters
    proteins_passing_filters <- sum(mask)
    heatmap_debug_log(paste("Proteins passing p-value filters:", proteins_passing_filters), 1)

    final_count <- sum(mask)
    heatmap_debug_log(paste("Protein selection from this function:", final_count, "proteins selected"), 1)

    return(mask)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in robust protein selection:", e$message), 1)

    # Return all FALSE mask in case of error
    emergency_mask <- rep(FALSE, nrow(dm))
    heatmap_debug_log("Error fallback: no proteins selected", 1)
    return(emergency_mask)
  })
}


# ========================================
# Plot Grid Integration
# ========================================

#' Get current plot object based on tab selection (mirrors download logic)
#' @param input Shiny input object
#' @param heatmap_fixed_expression Reactive for expression heatmap
#' @param heatmap_fixed_protein_correlation Reactive for protein correlation heatmap
#' @param heatmap_fixed_sample_correlation Reactive for sample correlation heatmap
#' @param heatmap_fixed_basemean Reactive for basemean heatmap
#' @param heatmap_fixed_abundance_ratio Reactive for abundance ratio heatmap
#' @param debug_log Debug logging function
#' @return List with status, plot object, and metadata
get_current_heatmap_for_grid <- function(input, heatmap_fixed_expression,
                                         heatmap_fixed_protein_correlation,
                                         heatmap_fixed_sample_correlation,
                                         heatmap_fixed_basemean,
                                         heatmap_fixed_abundance_ratio, debug_log) {

  heatmap_debug_log("Getting current heatmap for grid integration", 2)

  tryCatch({
    current_tab <- input$heatmap_unified_tabs %||% "grid_expr_corr"

    heatmap_debug_log(paste("Grid integration - Tab:", current_tab), 2)

    # Determine plot object and type based on current tab
    plot_result <- switch(current_tab,

                          "grid_expr_corr" = {
                            # Expression + Protein Correlation Grid (horizontal)
                            fixed_expr <- heatmap_fixed_expression()
                            fixed_prot_corr <- heatmap_fixed_protein_correlation()

                            if (!is.null(fixed_expr)) {
                              if (!is.null(fixed_prot_corr)) {
                                # Create horizontal combination
                                combined_obj <- fixed_expr + fixed_prot_corr
                                list(status = "success", type = "complex_combined", object = combined_obj, title = "Expression_Correlation_Grid")
                              } else {
                                # Only expression available
                                list(status = "success", type = "complex_single", object = fixed_expr, title = "Expression_Correlation_Grid")
                              }
                            } else {
                              list(status = "error", message = "No heatmap available for Expression + Correlation Grid. Please create heatmaps first.")
                            }
                          },

                          "grid_expr_sample" = {
                            # Expression + Sample Correlation Grid (vertical)
                            fixed_expr <- heatmap_fixed_expression()
                            fixed_sample_corr <- heatmap_fixed_sample_correlation()

                            if (!is.null(fixed_expr)) {
                              if (!is.null(fixed_sample_corr)) {
                                # Create VERTICAL combination (expression on top, sample correlation below)
                                combined_obj <- fixed_expr %v% fixed_sample_corr
                                list(status = "success", type = "complex_combined", object = combined_obj, title = "Expression_Sample_Grid")
                              } else {
                                # Only expression available
                                list(status = "success", type = "complex_single", object = fixed_expr, title = "Expression_Sample_Grid")
                              }
                            } else {
                              list(status = "error", message = "No heatmap available for Expression + Sample Grid. Please create heatmaps first.")
                            }
                          },

                          "expression_tab" = {
                            fixed_expr <- heatmap_fixed_expression()
                            if (!is.null(fixed_expr)) {
                              list(status = "success", type = "complex_single", object = fixed_expr, title = "Expression_Only")
                            } else {
                              list(status = "error", message = "No expression heatmap available. Create heatmap in grid tab first.")
                            }
                          },

                          "protein_cor_tab" = {
                            fixed_prot <- heatmap_fixed_protein_correlation()
                            if (!is.null(fixed_prot)) {
                              list(status = "success", type = "complex_single", object = fixed_prot, title = "Protein_Correlation")
                            } else {
                              list(status = "error", message = "No protein correlation heatmap available. Create heatmap in grid tab first.")
                            }
                          },

                          "sample_cor_tab" = {
                            fixed_sample <- heatmap_fixed_sample_correlation()
                            if (!is.null(fixed_sample)) {
                              list(status = "success", type = "complex_single", object = fixed_sample, title = "Sample_Correlation")
                            } else {
                              list(status = "error", message = "No sample correlation heatmap available. Create heatmap in grid tab first.")
                            }
                          },

                          "basemean_tab" = {
                            fixed_basemean <- heatmap_fixed_basemean()
                            if (!is.null(fixed_basemean)) {
                              list(status = "success", type = "complex_single", object = fixed_basemean, title = "Basemean_Only")
                            } else {
                              list(status = "error", message = "No basemean heatmap available. Enable basemean in grid tab and create heatmaps first.")
                            }
                          },

                          "abundance_ratio_tab" = {
                            fixed_ratio <- heatmap_fixed_abundance_ratio()
                            if (!is.null(fixed_ratio)) {
                              list(status = "success", type = "complex_single", object = fixed_ratio, title = "Abundance_Ratio")
                            } else {
                              list(status = "error", message = "No abundance ratio heatmap available. Enable abundance ratio in grid tab and create heatmaps first.")
                            }
                          },

                          {
                            list(status = "error", message = paste("Cannot get plot from tab:", current_tab))
                          }
    )

    heatmap_debug_log(paste("Plot retrieval result:", plot_result$status, "- Type:", plot_result$type %||% "unknown"), 2)
    return(plot_result)

  }, error = function(e) {
    heatmap_debug_log(paste("Error in get_current_heatmap_for_grid:", e$message), 1)
    return(list(status = "error", message = paste("Error retrieving plot:", e$message)))
  })
}

#' Validate ComplexHeatmap object
#' @param heatmap_obj Object to validate
#' @return Logical indicating if object is valid ComplexHeatmap
is_valid_complexheatmap <- function(heatmap_obj) {
  if (is.null(heatmap_obj)) return(FALSE)

  # Check if it's a ComplexHeatmap object
  is_heatmap <- methods::is(heatmap_obj, "Heatmap")
  is_heatmap_list <- methods::is(heatmap_obj, "HeatmapList")

  return(is_heatmap || is_heatmap_list)
}

#' Get descriptive name for current heatmap configuration
#' @param input Shiny input object
#' @return Character string describing current configuration
get_heatmap_config_description <- function(input) {
  use_single <- isTRUE(input$hm_download_single_toggle)

  if (use_single) {
    single_tab <- input$heatmap_single_tabs
    return(switch(single_tab,
                  "expression_tab" = "Expression",
                  "protein_cor_tab" = "Protein Correlation",
                  "sample_cor_tab" = "Sample Correlation",
                  "basemean_tab" = "Basemean",
                  "abundance_ratio_tab" = "Abundance Ratio",
                  "Unknown"))
  } else {
    grid_tab <- input$heatmap_grid_tabs
    return(switch(grid_tab,
                  "grid_expr_corr" = "Expression + Correlation Grid",
                  "grid_expr_sample" = "Expression + Sample Grid",
                  "Grid"))
  }
}
# ==============================================================================
# Alignment Verification
# ==============================================================================
# verify_heatmap_alignment
# Purpose:
#   Checks that the protein correlation matrix rows/cols are aligned with the
#   expression matrix rows after a heatmap creation run.
# Called by:
#   run_heatmap_creation() in Heatmap_observers.R, wrapped in tryCatch so that
#   any failure is non-fatal and logged at level 2 only.
# Note:
#   This is a diagnostic helper; its output is informational only and does not
#   affect reactive state.
# ==============================================================================
verify_heatmap_alignment <- function() {
  tryCatch({
    plots <- heatmap_plots()
    if (is.null(plots)) return(invisible(NULL))

    expr_ht <- plots$expression %||% plots$expr
    protein_ht <- plots$protein_cor %||% plots$prot

    if (is.null(expr_ht)) return(invisible(NULL))

    expr_core <- extract_core_heatmap(expr_ht)
    expression_row_names <- rownames(expr_core@matrix)

    if (!is.null(protein_ht)) {
      protein_cor_row_names <- rownames(protein_ht@matrix)
      protein_cor_col_names <- colnames(protein_ht@matrix)
      rows_aligned <- identical(protein_cor_row_names, expression_row_names)
      cols_aligned <- identical(protein_cor_col_names, expression_row_names)
      heatmap_debug_log(paste("Protein correlation - Rows aligned:", rows_aligned), 2)
      heatmap_debug_log(paste("Protein correlation - Cols aligned:", cols_aligned), 2)
    }
  }, error = function(e) {
    heatmap_debug_log(paste("Error in alignment verification:", e$message), 1)
  })
}
