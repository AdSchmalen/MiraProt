# GSEA_module_logic.R
#
# Purpose:
#   Pure logic and reusable helper functions for the GSEA module. Contains no
#   Shiny reactives, no server-side code, and no side effects beyond file reads
#   required for computation.
#
# Architecture:
#   This file is sourced by GSEA_module.R and provides the statistical engine
#   that underpins ranking, tie resolution, validation, and visualization
#   helpers. All functions are safe to call outside a Shiny session.
#
# Structure:
#   1.  Module-level debug logging helpers
#   2.  Library loading
#   3.  Data preparation (createGSEA_dataframe)
#   4.  Robust matrix statistics (rowMeans_robust, rowVars_robust, rowSds_robust)
#   5.  Statistical ranking methods (S2N, ttest, ratio, gsea_diff, log2_ratio,
#       SoR, BWS, WAD, FCROS, est_hyper, MWT, MSD)
#   6.  Main rank calculation dispatcher (calc_ranks_GSEA)
#   7.  PADOG gene weighting
#   8.  Ranking wrapper functions (compute_custom_ranks_GSEA,
#       compute_precalculated_ranks_GSEA)
#   9.  Validation (validate_ranking_vector)
#   10. Tie resolution (hierarchical, jitter, automatic)
#   11. Seed generation for reproducible jitter
#   12. Text wrapping and label formatting
#   13. P-value smart formatting
#   14. Intelligent decimal place calculation
#   15. Display tie resolution
#   16. Miscellaneous helpers (validate_abundance_type,
#       normalize_validation_logic, get_validation_data_for_imputed_content)
#
# Developer notes:
#   - gsea_debug_log(message, level, DEBUG_LEVEL) is the module-level logging
#     function. High-level entry points accept an optional debug_log parameter.
#   - Level 1: important operational messages (ranking started, genes loaded).
#   - Level 2: verbose tracing (per-method details, tie counts).
#   - Avoid debug calls inside tight loops (e.g., per-gene iterations).

# ============================================================
# Module-Level Debug Logging
# ============================================================

#' Create a debug log function bound to a specific debug level
#'
#' @param DEBUG_LEVEL Integer; minimum level for messages to be printed.
#' @return A function(message, level = 1) that logs to the console.
create_gsea_debug_log <- function(DEBUG_LEVEL = 1) {
  function(message, level = 1) {
    if (DEBUG_LEVEL >= level) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      line <- paste0("[ GSEA MODULE ", timestamp, " ] ", message)
      cat(line, "\n")
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) rec(line)
    }
  }
}

#' Module-level logging function (standalone, no closure)
#'
#' @param message Character; the message to log.
#' @param level Integer; message verbosity level.
#' @param DEBUG_LEVEL Integer; current debug threshold (defaults to globalenv binding).
gsea_debug_log <- function(message, level = 1,
                           DEBUG_LEVEL = tryCatch(
                             get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
                             error = function(e) 0L)) {
  rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
  if (is.function(rec)) {
    rec(level, "GSEA MODULE", message)
  } else {
    if (is.numeric(DEBUG_LEVEL) && DEBUG_LEVEL >= level) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      cat(paste0("[ GSEA MODULE ", timestamp, " ] ", message), "\n")
    }
  }
}

# ============================================================
# Library Loading
# ============================================================

#' Load all libraries required by the GSEA module
#'
#' Checks availability of required packages and attaches any that are not yet
#' on the search path. Packages already loaded by bootstrap.R are skipped to
#' avoid redundant library() overhead at session startup.
#'
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Logical; TRUE if all critical libraries are available, FALSE if
#'   clusterProfiler (the critical dependency) is missing.
gsea_load_required_libraries <- function(DEBUG_LEVEL = 1) {
  required_libs <- c("clusterProfiler", "parallel", "pracma",
                     "enrichplot", "fgsea")
  missing_libs <- character()

  for (lib in required_libs) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      missing_libs <- c(missing_libs, lib)
    } else if (!paste0("package:", lib) %in% search()) {
      # Only call library() if the package is not yet attached; packages
      # pre-loaded by bootstrap.R are already on the search path and do not
      # need to be re-attached here.
      suppressPackageStartupMessages(library(lib, character.only = TRUE))
    }
  }

  if (length(missing_libs) > 0) {
    gsea_debug_log(
      paste("Required libraries not available:", paste(missing_libs, collapse = ", ")),
      1, DEBUG_LEVEL
    )
    if ("clusterProfiler" %in% missing_libs) return(FALSE)
  }

  TRUE
}

# ============================================================
# Robust Matrix Statistics
# ============================================================

rowMeans_robust <- function(x) {
  if (is.null(x) || nrow(x) == 0 || ncol(x) == 0) return(numeric(0))
  rowMeans(x, na.rm = TRUE)
}

rowVars_robust <- function(x) {
  if (is.null(x) || nrow(x) == 0 || ncol(x) == 0) return(numeric(0))
  apply(x, 1, function(row) {
    valid_vals <- row[!is.na(row)]
    if (length(valid_vals) < 2) return(0.001)
    max(var(valid_vals), 0.001)
  })
}

rowSds_robust <- function(x) {
  if (is.null(x) || nrow(x) == 0 || ncol(x) == 0) return(numeric(0))
  apply(x, 1, function(row) {
    valid_vals <- row[!is.na(row)]
    if (length(valid_vals) < 2) return(0.001)
    max(sd(valid_vals), 0.001)
  })
}

rowMedians_robust <- function(x) {
  if (is.null(x) || nrow(x) == 0 || ncol(x) == 0) return(numeric(0))
  apply(x, 1, function(row) {
    valid_vals <- row[!is.na(row)]
    if (length(valid_vals) == 0) return(0)
    median(valid_vals)
  })
}

# ============================================================
# Data Preparation
# ============================================================

#' Build a GSEA-ready data frame from raw abundance data
#'
#' Extracts the relevant sample columns, optionally applies imputation,
#' filters rows by a minimum valid-value rule, and returns the data matrix
#' alongside a group assignment vector.
#'
#' @param raw Data frame; the full data table.
#' @param def Data frame; the metadata / data definition table.
#' @param nums Character vector; numerator sample names.
#' @param dens Character vector; denominator sample names.
#' @param ref_val Character; selected abundance type (Content column in def).
#' @param id_col Character; identifier column name (Options column in def).
#' @param valid_n Integer or NA; minimum valid values per gene per group.
#' @param valid_g Character; validation logic ("In one group", "In total",
#'   "In each group").
#' @param impute_list Named list; optional imputation values per gene ID.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Named list with elements `df` (data frame) and `group` (integer
#'   vector: 0 = numerator, 1 = denominator), or NULL on error.
createGSEA_dataframe <- function(raw, def, nums, dens, ref_val, id_col,
                                  valid_n, valid_g, impute_list, DEBUG_LEVEL = 1) {
  tryCatch({
    gsea_debug_log("Creating GSEA dataframe", 2, DEBUG_LEVEL)

    if (is.null(raw) || is.null(def) || nrow(raw) == 0 || nrow(def) == 0) {
      gsea_debug_log("Invalid input data for GSEA dataframe creation", 1, DEBUG_LEVEL)
      return(NULL)
    }

    def_content <- as.character(def$Content)
    def_sample  <- as.character(def$Sample)
    def_options <- as.character(def$Options)

    id_idx <- which(def_options == id_col)[1]
    if (is.na(id_idx)) {
      gsea_debug_log("Identifier column not found", 1, DEBUG_LEVEL)
      return(NULL)
    }

    ref_idx <- which(def_content == ref_val)
    if (length(ref_idx) == 0) {
      gsea_debug_log(paste0("No reference value columns found for '", ref_val, "'"), 1, DEBUG_LEVEL)
      return(NULL)
    }

    nums_idx <- ref_idx[def_sample[ref_idx] %in% nums]
    dens_idx <- ref_idx[def_sample[ref_idx] %in% dens]
    if (length(nums_idx) == 0 || length(dens_idx) == 0) {
      gsea_debug_log("Sample groups not found in data", 1, DEBUG_LEVEL)
      return(NULL)
    }

    gene_ids   <- as.character(raw[, id_idx])
    valid_genes <- !is.na(gene_ids) & gene_ids != ""
    if (!any(valid_genes)) {
      gsea_debug_log("No valid gene identifiers found", 1, DEBUG_LEVEL)
      return(NULL)
    }

    data_cols_idx <- c(nums_idx, dens_idx)
    data_matrix   <- as.matrix(raw[valid_genes, data_cols_idx, drop = FALSE])

    if (!is.null(impute_list) && length(impute_list) > 0) {
      gsea_debug_log("Applying imputation to ranking matrix", 2, DEBUG_LEVEL)
      row_ids <- gene_ids[valid_genes]
      for (i in seq_len(nrow(data_matrix))) {
        na_idx <- is.na(data_matrix[i, ])
        if (any(na_idx)) {
          rid  <- row_ids[i]
          repl <- impute_list[[rid]]
          if (!is.null(repl) && length(repl) >= ncol(data_matrix)) {
            data_matrix[i, na_idx] <- repl[na_idx]
          }
        }
      }
    }

    normalize_logic <- function(vl) {
      if (is.null(vl) || !nzchar(vl)) return("one")
      vl <- tolower(trimws(gsub("[ _-]+", " ", vl)))
      if (vl %in% c("in total", "total")) "total"
      else if (vl %in% c("in one group", "one group", "one")) "one"
      else "each"
    }
    logic_key   <- normalize_logic(valid_g)
    valid_n_num <- suppressWarnings(as.integer(valid_n))
    do_filter   <- isTRUE(valid_n_num >= 1)

    is_imputed <- grepl("^\\s*Imputed\\b", ref_val, ignore.case = TRUE)
    if (is_imputed) {
      original_content <- sub("^\\s*Imputed\\s+", "", ref_val, ignore.case = TRUE)
      map_to_original  <- function(idx_vec) {
        if (length(idx_vec) == 0) return(idx_vec)
        vapply(idx_vec, function(ix) {
          sname <- def_sample[ix]
          cand  <- which(def_content == original_content & def_sample == sname)
          if (length(cand) >= 1) cand[1] else ix
        }, integer(1))
      }
      val_nums_idx <- map_to_original(nums_idx)
      val_dens_idx <- map_to_original(dens_idx)
      gsea_debug_log(
        paste0("Validation uses original content '", original_content, "'"),
        2, DEBUG_LEVEL
      )
    } else {
      val_nums_idx <- nums_idx
      val_dens_idx <- dens_idx
    }

    keep_rows <- rep(TRUE, sum(valid_genes))

    if (do_filter) {
      val_num_mat <- as.matrix(raw[valid_genes, val_nums_idx, drop = FALSE])
      val_den_mat <- as.matrix(raw[valid_genes, val_dens_idx, drop = FALSE])
      vnum <- rowSums(!is.na(val_num_mat) & is.finite(val_num_mat) & (val_num_mat > 0))
      vden <- rowSums(!is.na(val_den_mat) & is.finite(val_den_mat) & (val_den_mat > 0))
      keep_rows <- switch(
        logic_key,
        total = (vnum + vden) >= valid_n_num,
        one   = (vnum >= valid_n_num) | (vden >= valid_n_num),
        each  = (vnum >= valid_n_num) & (vden >= valid_n_num)
      )
      keep_rows[is.na(keep_rows)] <- FALSE
    }

    before_n          <- nrow(data_matrix)
    data_matrix       <- data_matrix[keep_rows, , drop = FALSE]
    gene_ids_filtered <- gene_ids[valid_genes][keep_rows]
    after_n           <- nrow(data_matrix)

    gsea_debug_log(
      paste0("Validation: logic=", logic_key,
             " | valid_n=", ifelse(is.na(valid_n_num), "", valid_n_num),
             " | kept ", after_n, "/", before_n, " genes"),
      1, DEBUG_LEVEL
    )

    if (after_n == 0) {
      gsea_debug_log("No genes passed validation criteria", 1, DEBUG_LEVEL)
      return(NULL)
    }

    group_vector <- c(rep(0, length(nums_idx)), rep(1, length(dens_idx)))

    result_df <- data.frame(
      Gene        = gene_ids_filtered,
      data_matrix,
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )

    gsea_debug_log(
      paste("GSEA dataframe created with", nrow(result_df), "genes"),
      1, DEBUG_LEVEL
    )

    list(df = result_df, group = group_vector)

  }, error = function(e) {
    gsea_debug_log(paste("Error creating GSEA dataframe:", e$message), 1, DEBUG_LEVEL)
    NULL
  })
}

# ============================================================
# Statistical Ranking Methods
# ============================================================

S2N <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gr0  <- group == 0; gr1 <- group == 1
    mean0 <- rowMeans_robust(data[, gr0, drop = FALSE])
    mean1 <- rowMeans_robust(data[, gr1, drop = FALSE])
    sd0   <- rowSds_robust(data[, gr0, drop = FALSE])
    sd1   <- rowSds_robust(data[, gr1, drop = FALSE])
    s2n   <- (mean0 - mean1) / (sd0 + sd1 + 1e-9)
    s2n[!is.finite(s2n)] <- 0
    s2n
  }, error = function(e) {
    gsea_debug_log(paste("Error in S2N:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

ttest <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gr0  <- group == 0; gr1 <- group == 1
    n0   <- sum(gr0);   n1  <- sum(gr1)
    mean0 <- rowMeans_robust(data[, gr0, drop = FALSE])
    mean1 <- rowMeans_robust(data[, gr1, drop = FALSE])
    var0  <- rowVars_robust(data[, gr0, drop = FALSE])
    var1  <- rowVars_robust(data[, gr1, drop = FALSE])
    se    <- sqrt(var0 / n0 + var1 / n1)
    tstat <- (mean0 - mean1) / se
    tstat[!is.finite(tstat)] <- 0
    tstat
  }, error = function(e) {
    gsea_debug_log(paste("Error in ttest:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

ratio <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gr0   <- group == 0; gr1 <- group == 1
    mean0 <- rowMeans_robust(data[, gr0, drop = FALSE])
    mean1 <- rowMeans_robust(data[, gr1, drop = FALSE])
    mean1[mean1 == 0] <- 1e-9
    r <- mean0 / mean1
    r[!is.finite(r)] <- 1
    r
  }, error = function(e) {
    gsea_debug_log(paste("Error in ratio:", e$message), 1, DEBUG_LEVEL)
    rep(1, nrow(data))
  })
}

gsea_diff <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gr0   <- group == 0; gr1 <- group == 1
    mean0 <- rowMeans_robust(data[, gr0, drop = FALSE])
    mean1 <- rowMeans_robust(data[, gr1, drop = FALSE])
    diff_val <- mean0 - mean1
    diff_val[!is.finite(diff_val)] <- 0
    diff_val
  }, error = function(e) {
    gsea_debug_log(paste("Error in gsea_diff:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

log2_ratio <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gr0   <- group == 0; gr1 <- group == 1
    mean0 <- rowMeans_robust(data[, gr0, drop = FALSE])
    mean1 <- rowMeans_robust(data[, gr1, drop = FALSE])
    lv    <- log2((mean0 + 1e-9) / (mean1 + 1e-9))
    lv[!is.finite(lv)] <- 0
    lv
  }, error = function(e) {
    gsea_debug_log(paste("Error in log2_ratio:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

SoR <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    n_genes   <- nrow(data)
    sor_stats <- numeric(n_genes)
    gr0 <- group == 0; gr1 <- group == 1
    for (i in seq_len(n_genes)) {
      g     <- data[i, ]
      valid <- is.finite(g)
      if (sum(valid) < 3) { sor_stats[i] <- 0; next }
      r        <- rank(g[valid], ties.method = "average")
      g0       <- gr0[valid]
      rank_sum <- sum(r[g0])
      n0 <- sum(g0); n1 <- sum(!g0); N <- n0 + n1
      expected <- n0 * (N + 1) / 2
      sor_stats[i] <- (rank_sum - expected) / sqrt(n0 * n1 * (N + 1) / 12)
    }
    sor_stats[!is.finite(sor_stats)] <- 0
    sor_stats
  }, error = function(e) {
    gsea_debug_log(paste("Error in SoR:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

BWS <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    n_genes   <- nrow(data)
    bws_stats <- numeric(n_genes)
    eps       <- 1e-12

    for (i in seq_len(n_genes)) {
      g     <- data[i, ]
      valid <- is.finite(g)
      if (sum(valid) < 4) next
      x  <- g[valid & group == 0]; y <- g[valid & group == 1]
      nx <- length(x);              ny <- length(y)
      if (nx < 2 || ny < 2) next
      z  <- c(x, y)
      r  <- rank(z, ties.method = "average")
      u  <- r / length(z)
      ux <- u[seq_len(nx)]; uy <- u[(nx + 1):(nx + ny)]
      phi <- function(t) (t - 0.5) / sqrt(pmax(t * (1 - t), eps))
      bws_stats[i] <- mean(phi(ux)^2) + mean(phi(uy)^2)
    }

    bws_stats[!is.finite(bws_stats)] <- 0
    bws_stats
  }, error = function(e) {
    gsea_debug_log(paste("Error in BWS:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

WAD <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gr0   <- group == 0; gr1 <- group == 1
    mean0 <- rowMeans_robust(data[, gr0, drop = FALSE])
    mean1 <- rowMeans_robust(data[, gr1, drop = FALSE])
    AD    <- mean0 - mean1
    xbar  <- (mean0 + mean1) / 2
    row_min <- apply(data, 1, function(v) min(v[is.finite(v)], na.rm = TRUE))
    row_max <- apply(data, 1, function(v) max(v[is.finite(v)], na.rm = TRUE))
    denom   <- row_max - row_min
    denom[denom == 0] <- 1
    w   <- (xbar - row_min) / denom
    wad <- AD * w
    wad[!is.finite(wad)] <- 0
    wad
  }, error = function(e) {
    gsea_debug_log(paste("Error in WAD:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

FCROS <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gr0 <- which(group == 0); gr1 <- which(group == 1)
    k   <- length(gr0) * length(gr1)
    if (k == 0) return(rep(0, nrow(data)))
    fc_matrix <- matrix(NA_real_, nrow = nrow(data), ncol = k)
    col_idx   <- 1
    for (i in gr0) for (j in gr1) {
      fc_matrix[, col_idx] <- log2((data[, i] + 1e-9) / (data[, j] + 1e-9))
      col_idx <- col_idx + 1
    }
    trim  <- 0.1
    fcros <- apply(fc_matrix, 1, function(v) {
      v <- v[is.finite(v)]
      if (length(v) == 0) return(0)
      mean(v, trim = trim)
    })
    fcros[!is.finite(fcros)] <- 0
    fcros
  }, error = function(e) {
    gsea_debug_log(paste("Error in FCROS:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

#' Estimate empirical Bayes hyperparameters for the MWT statistic
#'
#' @param z Numeric vector; log-variance values.
#' @param D Numeric; degrees-of-freedom parameter.
#' @param debug_level Integer; logging verbosity.
#' @return Named list with elements s02 and d0.
est_hyper <- function(z, D, debug_level = 0) {
  tryCatch({
    z[is.na(z)] <- 0
    z_clean <- z[is.finite(z)]

    if (length(z_clean) < 3) {
      gsea_debug_log("Insufficient data for hyperparameter estimation", 1, debug_level)
      return(list(s02 = 1, d0 = 1))
    }
    if (!requireNamespace("pracma", quietly = TRUE)) {
      gsea_debug_log("pracma package required for MWT", 1, debug_level)
      return(list(s02 = 1, d0 = 1))
    }

    fun1 <- function(d0) var(z_clean, na.rm = TRUE) - pracma::psi(1, D / 2) - pracma::psi(1, d0 / 2)
    lim  <- fun1(100)

    if (is.na(lim) || lim < 0) {
      d0 <- 100
    } else {
      d0 <- tryCatch(
        uniroot(fun1, interval = c(0.01, 100))$root,
        error = function(e) {
          gsea_debug_log("Hyperparameter estimation failed, using default", 1, debug_level)
          100
        }
      )
    }

    s02 <- exp(mean(z_clean, na.rm = TRUE) - pracma::psi(0, D / 2) +
                 pracma::psi(0, d0 / 2) - log(d0 / D))
    if (!is.finite(s02) || s02 <= 0) s02 <- 1

    list(s02 = s02, d0 = d0)
  }, error = function(e) {
    gsea_debug_log(paste("Hyperparameter estimation error:", e$message), 1, debug_level)
    list(s02 = 1, d0 = 1)
  })
}

MWT <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gsea_debug_log("Calculating Moderated Welch Test", 2, DEBUG_LEVEL)
    gr0 <- group == 0; gr1 <- group == 1
    n0  <- sum(gr0);   n1  <- sum(gr1)
    m0  <- rowMeans(data[, gr0, drop = FALSE])
    m1  <- rowMeans(data[, gr1, drop = FALSE])
    v0  <- rowVars_robust(data[, gr0, drop = FALSE])
    v1  <- rowVars_robust(data[, gr1, drop = FALSE])
    s2  <- v0 / n0 + v1 / n1
    df  <- (s2^2) / ((v0^2) / (n0^2 * (n0 - 1)) + (v1^2) / (n1^2 * (n1 - 1)))
    df[!is.finite(df)] <- median(df[is.finite(df)])

    log_s2 <- log(s2)
    m      <- mean(log_s2)
    v      <- var(log_s2)
    d0     <- 2 / v
    s02    <- exp(m - digamma(d0 / 2) + log(d0 / 2))
    s2_mod <- (d0 * s02 + df * s2) / (d0 + df)
    mwt    <- (m0 - m1) / sqrt(s2_mod)
    mwt[!is.finite(mwt)] <- 0
    mwt
  }, error = function(e) {
    gsea_debug_log(paste("Error in MWT:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

MSD <- function(data, group, DEBUG_LEVEL = 1) {
  tryCatch({
    gr0   <- group == 0; gr1 <- group == 1
    n0    <- sum(gr0);   n1  <- sum(gr1)
    mean0 <- rowMeans_robust(data[, gr0, drop = FALSE])
    mean1 <- rowMeans_robust(data[, gr1, drop = FALSE])
    var0  <- rowVars_robust(data[, gr0, drop = FALSE])
    var1  <- rowVars_robust(data[, gr1, drop = FALSE])
    se    <- sqrt(var0 / n0 + var1 / n1)
    se[se == 0] <- 1
    logFC <- log2((mean0 + 1e-9) / (mean1 + 1e-9))
    Cval  <- abs(logFC) / se
    msd   <- ifelse(logFC > 0, Cval, -Cval)
    msd[!is.finite(msd)] <- 0
    msd
  }, error = function(e) {
    gsea_debug_log(paste("Error in MSD:", e$message), 1, DEBUG_LEVEL)
    rep(0, nrow(data))
  })
}

# ============================================================
# Main Rank Calculation Dispatcher
# ============================================================

#' Calculate ranking values for GSEA using the selected statistical method
#'
#' @param df Data frame; output of createGSEA_dataframe()$df.
#' @param group Integer vector; 0 = numerator, 1 = denominator.
#' @param method Character; method code from gsea_get_rank_methods().
#' @param abs_f Logical; take absolute values of ranking scores.
#' @param ties_f Logical; apply automatic tie resolution.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Data frame with columns Gene, Rank, FoldChange, sorted descending
#'   by Rank, or NULL on error.
calc_ranks_GSEA <- function(df, group, method, abs_f = FALSE, ties_f = FALSE, DEBUG_LEVEL = 1) {
  tryCatch({
    gsea_debug_log(paste("Calculating ranks with method:", method), 1, DEBUG_LEVEL)

    gene_names  <- as.character(df$Gene)
    data_matrix <- as.matrix(df[, -1])
    rownames(data_matrix) <- gene_names

    if (nrow(data_matrix) == 0 || ncol(data_matrix) == 0) {
      gsea_debug_log("Empty data matrix", 1, DEBUG_LEVEL)
      return(NULL)
    }

    raw_vals <- switch(method,
      "S2N"       = S2N(data_matrix, group, DEBUG_LEVEL),
      "ttest"     = ttest(data_matrix, group, DEBUG_LEVEL),
      "ratio"     = ratio(data_matrix, group, DEBUG_LEVEL),
      "diff"      = gsea_diff(data_matrix, group, DEBUG_LEVEL),
      "log2_ratio" = log2_ratio(data_matrix, group, DEBUG_LEVEL),
      "SoR"       = SoR(data_matrix, group, DEBUG_LEVEL),
      "BWS"       = BWS(data_matrix, group, DEBUG_LEVEL),
      "WAD"       = WAD(data_matrix, group, DEBUG_LEVEL),
      "FCROS"     = FCROS(data_matrix, group, DEBUG_LEVEL),
      "MWT"       = MWT(data_matrix, group, DEBUG_LEVEL),
      "MSD"       = MSD(data_matrix, group, DEBUG_LEVEL),
      {
        gsea_debug_log(paste("Unknown ranking method:", method), 1, DEBUG_LEVEL)
        return(NULL)
      }
    )

    if (is.null(raw_vals)) {
      gsea_debug_log("Ranking calculation returned NULL", 1, DEBUG_LEVEL)
      return(NULL)
    }

    gr0 <- group == 0; gr1 <- group == 1
    mean0 <- rowMeans(data_matrix[, gr0, drop = FALSE], na.rm = TRUE)
    mean1 <- rowMeans(data_matrix[, gr1, drop = FALSE], na.rm = TRUE)
    fold_changes <- mean0 / mean1
    fold_changes[!is.finite(fold_changes)] <- 1

    if (abs_f) raw_vals <- abs(raw_vals)

    ranking_vals <- raw_vals
    names(ranking_vals) <- gene_names

    if (ties_f) {
      gsea_debug_log("Applying automatic tie resolution", 2, DEBUG_LEVEL)
      secondary_metrics <- if (method %in% c("ratio", "log2_ratio")) {
        var0 <- apply(data_matrix[, gr0, drop = FALSE], 1, var, na.rm = TRUE)
        var1 <- apply(data_matrix[, gr1, drop = FALSE], 1, var, na.rm = TRUE)
        -(var0 + var1)
      } else {
        abs(log2(fold_changes))
      }
      ranking_vals <- resolve_ties_automatic(ranking_vals, secondary_metrics, gene_names, DEBUG_LEVEL)
    }

    ranks_df <- data.frame(
      Gene        = gene_names,
      Rank        = ranking_vals,
      FoldChange  = fold_changes,
      stringsAsFactors = FALSE
    )
    ranks_df <- ranks_df[order(ranks_df$Rank, decreasing = TRUE), ]

    gsea_debug_log(paste("Rank calculation completed with", nrow(ranks_df), "genes"), 1, DEBUG_LEVEL)
    ranks_df

  }, error = function(e) {
    gsea_debug_log(paste("Error in calc_ranks_GSEA:", e$message), 1, DEBUG_LEVEL)
    NULL
  })
}

# ============================================================
# PADOG Gene Weighting
# ============================================================

#' Calculate PADOG gene frequency weights from a GMT file
#'
#' @param gene_set_file Character; path to the .gmt file.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Data frame with columns gene and weight, or NULL on error.
calculate_gene_weights_GSEA <- function(gene_set_file, DEBUG_LEVEL = 1) {
  tryCatch({
    gsea_debug_log("Calculating PADOG gene weights", 2, DEBUG_LEVEL)

    if (!file.exists(gene_set_file)) {
      gsea_debug_log("Gene set file not found for PADOG weighting", 1, DEBUG_LEVEL)
      return(NULL)
    }

    gene_sets <- clusterProfiler::read.gmt(gene_set_file)
    if (is.null(gene_sets) || nrow(gene_sets) == 0) {
      gsea_debug_log("Empty gene set file", 1, DEBUG_LEVEL)
      return(NULL)
    }

    gene_counts <- table(gene_sets$gene)
    max_f <- max(gene_counts); min_f <- min(gene_counts)

    if (max_f == min_f) {
      gsea_debug_log("No gene appears multiple times - no weighting applied", 1, DEBUG_LEVEL)
      gene_weights <- rep(1, length(gene_counts))
    } else {
      gene_weights <- sqrt((max_f - gene_counts) / (max_f - min_f))
    }

    result <- data.frame(
      gene   = names(gene_weights),
      weight = as.numeric(gene_weights),
      stringsAsFactors = FALSE
    )
    gsea_debug_log(paste("PADOG weights calculated for", nrow(result), "genes"), 2, DEBUG_LEVEL)
    result

  }, error = function(e) {
    gsea_debug_log(paste("Error calculating PADOG weights:", e$message), 1, DEBUG_LEVEL)
    NULL
  })
}

#' Apply PADOG weights to a ranking data frame
#'
#' @param ranks_df Data frame with columns Gene, Rank, FoldChange.
#' @param gene_set_file Character; path to the .gmt file.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Modified ranks_df with weighted ranks, sorted descending.
apply_padog_weighting <- function(ranks_df, gene_set_file, DEBUG_LEVEL = 1) {
  tryCatch({
    weights <- calculate_gene_weights_GSEA(gene_set_file, DEBUG_LEVEL)

    if (is.null(weights)) {
      gsea_debug_log("PADOG weighting failed - returning original ranks", 1, DEBUG_LEVEL)
      return(ranks_df)
    }

    common_genes <- intersect(ranks_df$Gene, weights$gene)
    if (length(common_genes) == 0) {
      gsea_debug_log("No common genes for PADOG weighting", 1, DEBUG_LEVEL)
      return(ranks_df)
    }

    for (gene in common_genes) {
      gene_idx   <- which(ranks_df$Gene == gene)
      weight_idx <- which(weights$gene == gene)
      if (length(gene_idx) > 0 && length(weight_idx) > 0) {
        ranks_df$Rank[gene_idx] <- ranks_df$Rank[gene_idx] * weights$weight[weight_idx]
      }
    }

    ranks_df <- ranks_df[order(ranks_df$Rank, decreasing = TRUE), ]
    gsea_debug_log(paste("PADOG weighting applied to", length(common_genes), "genes"), 1, DEBUG_LEVEL)
    ranks_df

  }, error = function(e) {
    gsea_debug_log(paste("Error applying PADOG weighting:", e$message), 1, DEBUG_LEVEL)
    ranks_df
  })
}

# ============================================================
# Ranking Wrapper Functions
# ============================================================

#' Compute custom gene rankings from raw abundance data
#'
#' @param raw Data frame; the raw data table.
#' @param def Data frame; the metadata / data definition table.
#' @param nums Character vector; numerator sample names.
#' @param dens Character vector; denominator sample names.
#' @param id_col Character; identifier column option name.
#' @param ref_val Character; reference abundance type.
#' @param method Character; method code.
#' @param abs_f Logical; use absolute values.
#' @param ties_f Logical; resolve ties.
#' @param padog_f Logical; apply PADOG weighting.
#' @param gene_set_file Character; path to GMT file (required for PADOG).
#' @param valid_n Integer or NA; minimum valid values.
#' @param valid_g Character; validation logic string.
#' @param impute_list Named list; optional imputation values.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Named list with elements Ranks and FC (named numeric vectors), or NULL.
compute_custom_ranks_GSEA <- function(raw, def, nums, dens, id_col, ref_val, method,
                                       abs_f, ties_f, padog_f, gene_set_file,
                                       valid_n, valid_g, impute_list = NULL,
                                       DEBUG_LEVEL = 1) {
  tryCatch({
    gsea_debug_log("Starting custom rank computation", 1, DEBUG_LEVEL)

    ranking_raw <- raw
    if (!is.null(raw) &&
        !is.null(def) &&
        "Transformation" %in% names(def) &&
        "Content" %in% names(def) &&
        "Column" %in% names(def)) {
      ref_rows <- which(as.character(def$Content) == ref_val)
      if (length(ref_rows) > 0) {
        raw_col_idx <- match(as.character(def$Column[ref_rows]), names(ranking_raw))
        valid_map <- !is.na(raw_col_idx)
        if (any(valid_map)) {
          ref_idx <- as.integer(raw_col_idx[valid_map])
          ref_transform <- as.character(def$Transformation[ref_rows][valid_map])

          needs_retransform <- !is.na(ref_transform) &
            nzchar(trimws(ref_transform)) &
            tolower(trimws(ref_transform)) != "none"

          if (any(needs_retransform)) {
            retransform_fun <- get0("retransform_data_global", mode = "function", inherits = TRUE)
            if (is.function(retransform_fun)) {
              gsea_debug_log("Retransforming custom-ranking abundance columns based on metadata", 1, DEBUG_LEVEL)
              ranking_raw <- retransform_fun(
                ranking_raw,
                ref_idx[needs_retransform],
                ref_transform[needs_retransform]
              )
            } else {
              gsea_debug_log("retransform_data_global not found; using original custom-ranking abundance values", 1, DEBUG_LEVEL)
            }
          }
        }
      }
    }

    gsea_data <- createGSEA_dataframe(ranking_raw, def, nums, dens, ref_val, id_col,
                                      valid_n, valid_g, impute_list, DEBUG_LEVEL)
    if (is.null(gsea_data)) {
      gsea_debug_log("Failed to create GSEA dataframe", 1, DEBUG_LEVEL)
      return(NULL)
    }

    ranks_df <- calc_ranks_GSEA(gsea_data$df, gsea_data$group, method,
                                 abs_f, ties_f, DEBUG_LEVEL)
    if (is.null(ranks_df)) {
      gsea_debug_log("Failed to calculate ranks", 1, DEBUG_LEVEL)
      return(NULL)
    }

    if (padog_f && !is.null(gene_set_file) && file.exists(gene_set_file)) {
      ranks_df <- apply_padog_weighting(ranks_df, gene_set_file, DEBUG_LEVEL)
    }

    valid_entries <- !is.na(ranks_df$Rank) & is.finite(ranks_df$Rank) &
      !is.na(ranks_df$Gene) & ranks_df$Gene != ""
    ranks_df <- ranks_df[valid_entries, ]

    if (nrow(ranks_df) == 0) {
      gsea_debug_log("No valid ranks after filtering", 1, DEBUG_LEVEL)
      return(NULL)
    }

    if (any(duplicated(ranks_df$Gene))) {
      dup_n <- sum(duplicated(ranks_df$Gene))
      before_n <- nrow(ranks_df)
      ranks_df <- ranks_df[order(ranks_df$Rank, decreasing = TRUE), ]
      ranks_df <- ranks_df[!duplicated(ranks_df$Gene), ]
      after_n <- nrow(ranks_df)
      gsea_debug_log(
        sprintf("Removed %d duplicate gene rows; kept highest rank per gene (%d -> %d)",
                dup_n, before_n, after_n),
        1,
        DEBUG_LEVEL
      )
    }

    gsea_debug_log(paste("Custom rank computation completed:", nrow(ranks_df), "genes"), 1, DEBUG_LEVEL)

    list(
      Ranks = setNames(ranks_df$Rank, ranks_df$Gene),
      FC    = setNames(ranks_df$FoldChange, ranks_df$Gene)
    )

  }, error = function(e) {
    gsea_debug_log(paste("Error in compute_custom_ranks_GSEA:", e$message), 1, DEBUG_LEVEL)
    NULL
  })
}

#' Compute gene rankings from precalculated fold change and p-value columns
#'
#' @param raw Data frame; the raw data table.
#' @param def Data frame; the metadata / data definition table.
#' @param ab_ratio_col Character; abundance ratio column name.
#' @param pval_col Character; p-value column name.
#' @param id_col Character; identifier column option name.
#' @param metric Character; ranking metric code ("log2(FC)",
#'   "log2(FC) x -log10(p)", "-log10(p)").
#' @param abs_f Logical; use absolute values.
#' @param ties_f Logical; resolve ties.
#' @param padog_f Logical; apply PADOG weighting.
#' @param gene_set_file Character or NULL; path to GMT file for PADOG.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Named list with elements Ranks and FC (named numeric vectors), or NULL.
compute_precalculated_ranks_GSEA <- function(raw, def, ab_ratio_col, pval_col, id_col,
                                              metric, abs_f, ties_f, padog_f = FALSE,
                                              gene_set_file = NULL, DEBUG_LEVEL = 1) {
  tryCatch({
    gsea_debug_log("Starting precalculated rank computation", 1, DEBUG_LEVEL)

    ratio_idx <- which(def$Column == ab_ratio_col)[1]
    pval_idx  <- which(def$Column == pval_col)[1]
    id_idx    <- which(def$Options == id_col)[1]

    if (is.na(ratio_idx) || is.na(pval_idx) || is.na(id_idx)) {
      gsea_debug_log("Required columns not found", 1, DEBUG_LEVEL)
      return(NULL)
    }

    gene_ids     <- as.character(raw[, id_idx])
    fold_changes <- as.numeric(raw[, ratio_idx])
    p_values     <- as.numeric(raw[, pval_idx])

    df <- data.frame(
      Gene   = gene_ids,
      FC     = fold_changes,
      PValue = p_values,
      stringsAsFactors = FALSE
    )

    valid_entries <- !is.na(df$Gene) & df$Gene != "" &
      !is.na(df$FC)     & is.finite(df$FC) &
      !is.na(df$PValue) & is.finite(df$PValue) & df$PValue > 0
    df <- df[valid_entries, ]

    if (nrow(df) == 0) {
      gsea_debug_log("No valid data for precalculated ranks", 1, DEBUG_LEVEL)
      return(NULL)
    }

    gsea_debug_log(paste("Valid precalculated data:", nrow(df), "genes"), 2, DEBUG_LEVEL)

    df$log2FC  <- log2(abs(df$FC) + 0.001)
    df$negLogP <- -log10(df$PValue)

    ranking_vals <- switch(metric,
      "log2(FC)"              = df$log2FC,
      "log2(FC) x -log10(p)" = df$log2FC * df$negLogP,
      "-log10(p)"             = df$negLogP,
      {
        gsea_debug_log("Unknown ranking metric", 1, DEBUG_LEVEL)
        return(NULL)
      }
    )

    if (abs_f) ranking_vals <- abs(ranking_vals)

    if (ties_f) {
      gsea_debug_log("Applying tie resolution for precalculated data", 2, DEBUG_LEVEL)
      secondary_metrics <- switch(metric,
        "log2(FC)"              = df$negLogP,
        "-log10(p)"             = df$log2FC,
        "log2(FC) x -log10(p)" = pmax(df$log2FC, df$negLogP),
        df$negLogP
      )
      ranking_vals <- resolve_ties_automatic(ranking_vals, secondary_metrics, df$Gene, DEBUG_LEVEL)
    }

    ranks_df <- data.frame(
      Gene       = df$Gene,
      Rank       = ranking_vals,
      FoldChange = df$FC,
      stringsAsFactors = FALSE
    )

    if (padog_f && !is.null(gene_set_file) && file.exists(gene_set_file)) {
      ranks_df <- apply_padog_weighting(ranks_df, gene_set_file, DEBUG_LEVEL)
    }

    if (any(duplicated(ranks_df$Gene))) {
      dup_n <- sum(duplicated(ranks_df$Gene))
      before_n <- nrow(ranks_df)
      ranks_df <- ranks_df[order(ranks_df$Rank, decreasing = TRUE), ]
      ranks_df <- ranks_df[!duplicated(ranks_df$Gene), ]
      after_n <- nrow(ranks_df)
      gsea_debug_log(
        sprintf("Removed %d duplicate gene rows; kept highest rank per gene (%d -> %d)",
                dup_n, before_n, after_n),
        1,
        DEBUG_LEVEL
      )
    }

    ranks_df <- ranks_df[order(ranks_df$Rank, decreasing = TRUE), ]
    gsea_debug_log(paste("Precalculated rank computation completed:", nrow(ranks_df), "genes"), 1, DEBUG_LEVEL)

    list(
      Ranks = setNames(ranks_df$Rank, ranks_df$Gene),
      FC    = setNames(ranks_df$FoldChange, ranks_df$Gene)
    )

  }, error = function(e) {
    gsea_debug_log(paste("Error in compute_precalculated_ranks_GSEA:", e$message), 1, DEBUG_LEVEL)
    NULL
  })
}

# ============================================================
# Validation
# ============================================================

#' Validate a named ranking vector for GSEA input
#'
#' @param ranks_vec Named numeric vector.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Logical; TRUE if the vector is valid.
validate_ranking_vector <- function(ranks_vec, DEBUG_LEVEL = 1) {
  if (is.null(ranks_vec) || length(ranks_vec) == 0) {
    gsea_debug_log("Empty ranking vector", 1, DEBUG_LEVEL)
    return(FALSE)
  }
  if (length(unique(ranks_vec)) <= 1) {
    gsea_debug_log("Ranking vector has insufficient variability", 1, DEBUG_LEVEL)
    return(FALSE)
  }
  if (any(duplicated(names(ranks_vec)))) {
    gsea_debug_log("Ranking vector contains duplicate gene names", 1, DEBUG_LEVEL)
    return(FALSE)
  }
  if (any(is.na(ranks_vec)) || any(!is.finite(ranks_vec))) {
    gsea_debug_log("Ranking vector contains invalid values", 1, DEBUG_LEVEL)
    return(FALSE)
  }
  gsea_debug_log(paste("Ranking vector validation passed:", length(ranks_vec), "genes"), 2, DEBUG_LEVEL)
  TRUE
}

# ============================================================
# Tie Resolution
# ============================================================

#' Generate a reproducible seed from gene names
#'
#' @param gene_names Character vector or NULL.
#' @param fallback_seed Integer; default 12345.
#' @return Integer seed value in [1, 2147483647].
generate_safe_seed <- function(gene_names = NULL, fallback_seed = 12345) {
  tryCatch({
    if (is.null(gene_names) || length(gene_names) == 0) return(fallback_seed)
    clean_names <- gsub("[^A-Za-z0-9]", "", as.character(gene_names))
    clean_names <- clean_names[nzchar(clean_names)]
    if (length(clean_names) == 0) return(fallback_seed)
    combined_string <- paste(sort(clean_names), collapse = "")
    char_codes <- utf8ToInt(substring(combined_string, 1, min(50, nchar(combined_string))))
    if (length(char_codes) > 0) {
      weighted_sum <- sum(char_codes * seq_along(char_codes))
      seed_value   <- abs(weighted_sum) %% 2147483646 + 1
      if (is.na(seed_value) || !is.finite(seed_value) ||
          seed_value < 1 || seed_value > 2147483647) {
        return(fallback_seed)
      }
      return(as.integer(seed_value))
    }
    fallback_seed
  }, error = function(e) fallback_seed)
}

#' Resolve ties using hierarchical method (secondary metric + jitter fallback)
#'
#' @param primary_values Numeric vector.
#' @param secondary_values Numeric vector of same length, or NULL.
#' @param gene_names Character vector or NULL; used for reproducible jitter seed.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Numeric vector with ties resolved.
resolve_ties_hierarchical <- function(primary_values, secondary_values = NULL,
                                       gene_names = NULL, DEBUG_LEVEL = 1) {
  gsea_debug_log("Resolving ties using hierarchical method", 2, DEBUG_LEVEL)

  result_values <- primary_values
  tied_indices  <- duplicated(primary_values) | duplicated(primary_values, fromLast = TRUE)
  n_resolved    <- 0

  if (any(tied_indices) &&
      !is.null(secondary_values) &&
      length(secondary_values) == length(primary_values)) {

    tied_values <- unique(primary_values[tied_indices])
    for (tied_val in tied_values) {
      tie_group <- which(primary_values == tied_val)
      if (length(tie_group) > 1) {
        secondary_order <- order(secondary_values[tie_group], decreasing = TRUE)
        epsilon_vals    <- seq(0, length(tie_group) - 1) * 1e-12
        result_values[tie_group] <- tied_val + epsilon_vals[order(secondary_order)]
        n_resolved <- n_resolved + length(tie_group) - 1
      }
    }
  }

  remaining_tied <- duplicated(result_values) | duplicated(result_values, fromLast = TRUE)
  if (any(remaining_tied)) {
    seed_value <- generate_safe_seed(gene_names, 12345)
    set.seed(seed_value)
    tied_values <- unique(result_values[remaining_tied])
    for (tied_val in tied_values) {
      tie_group <- which(result_values == tied_val)
      if (length(tie_group) > 1) {
        jitter_vals <- runif(length(tie_group), -1e-11, 1e-11)
        result_values[tie_group] <- tied_val + jitter_vals
        n_resolved <- n_resolved + length(tie_group) - 1
      }
    }
  }

  gsea_debug_log(paste("Hierarchical tie resolution:", n_resolved, "ties resolved"), 1, DEBUG_LEVEL)
  result_values
}

#' Resolve ties using minimal reproducible jitter
#'
#' @param primary_values Numeric vector.
#' @param gene_names Character vector or NULL; used for reproducible seed.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Numeric vector with ties resolved.
resolve_ties_reproducible_jitter <- function(primary_values, gene_names = NULL, DEBUG_LEVEL = 1) {
  gsea_debug_log("Resolving ties using reproducible jitter", 2, DEBUG_LEVEL)

  seed_value <- generate_safe_seed(gene_names, 12345)
  set.seed(seed_value)

  result_values <- primary_values
  tied_indices  <- duplicated(primary_values) | duplicated(primary_values, fromLast = TRUE)
  n_resolved    <- 0

  if (any(tied_indices)) {
    tied_values <- unique(primary_values[tied_indices])
    for (tied_val in tied_values) {
      tie_group <- which(primary_values == tied_val)
      if (length(tie_group) > 1) {
        jitter_range <- 1e-11
        jitter_vals  <- runif(length(tie_group), -jitter_range, jitter_range)
        result_values[tie_group] <- tied_val + jitter_vals
        n_resolved <- n_resolved + length(tie_group) - 1
      }
    }
  }

  gsea_debug_log(paste("Reproducible jitter:", n_resolved, "ties resolved"), 1, DEBUG_LEVEL)
  result_values
}

#' Automatically select and apply the best tie resolution method
#'
#' Uses hierarchical method when secondary metrics are available;
#' falls back to reproducible jitter otherwise.
#'
#' @param primary_values Numeric vector.
#' @param secondary_values Numeric vector of same length, or NULL.
#' @param gene_names Character vector or NULL.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Numeric vector with ties resolved.
resolve_ties_automatic <- function(primary_values, secondary_values = NULL,
                                    gene_names = NULL, DEBUG_LEVEL = 1) {
  gsea_debug_log("Automatic tie resolution method selection", 2, DEBUG_LEVEL)

  has_secondary <- !is.null(secondary_values) &&
    length(secondary_values) == length(primary_values) &&
    !all(is.na(secondary_values))

  if (has_secondary) {
    gsea_debug_log("Using hierarchical method (secondary metrics available)", 2, DEBUG_LEVEL)
    return(resolve_ties_hierarchical(primary_values, secondary_values, gene_names, DEBUG_LEVEL))
  }

  gsea_debug_log("Using reproducible jitter (no secondary metrics)", 2, DEBUG_LEVEL)
  resolve_ties_reproducible_jitter(primary_values, gene_names, DEBUG_LEVEL)
}

#' Resolve ties in ranking values for display purposes only
#'
#' This function is used for table display and does not affect analysis.
#'
#' @param ranking_values Numeric vector.
#' @param gene_names Character vector or NULL.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Numeric vector with display ties minimally resolved.
resolve_display_ties <- function(ranking_values, gene_names = NULL, DEBUG_LEVEL = 1) {
  if (length(ranking_values) == 0) return(ranking_values)
  if (!any(duplicated(ranking_values))) {
    gsea_debug_log("No ties detected for display", 2, DEBUG_LEVEL)
    return(ranking_values)
  }

  gsea_debug_log("Resolving ties for display", 2, DEBUG_LEVEL)

  result_values <- ranking_values
  tied_indices  <- duplicated(ranking_values) | duplicated(ranking_values, fromLast = TRUE)
  n_resolved    <- 0

  if (any(tied_indices)) {
    seed_value <- generate_safe_seed(gene_names, 12345)
    set.seed(seed_value)
    tied_values <- unique(ranking_values[tied_indices])
    for (tied_val in tied_values) {
      tie_group    <- which(ranking_values == tied_val)
      if (length(tie_group) > 1) {
        jitter_range <- max(1e-8, abs(tied_val) * 1e-10)
        jitter_vals  <- sort(runif(length(tie_group), -jitter_range, jitter_range), decreasing = TRUE)
        result_values[tie_group] <- tied_val + jitter_vals
        n_resolved <- n_resolved + length(tie_group) - 1
      }
    }
  }

  gsea_debug_log(paste("Display tie resolution:", n_resolved, "ties resolved"), 2, DEBUG_LEVEL)
  result_values
}

# ============================================================
# Intelligent Decimal Place Calculation
# ============================================================

#' Calculate the number of decimal places needed to distinguish ranking values
#'
#' @param ranking_values Numeric vector.
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @return Integer; number of decimal places (clamped to [2, 12]).
calculate_intelligent_decimals <- function(ranking_values, DEBUG_LEVEL = 1) {
  if (length(ranking_values) == 0) return(4)
  ranking_values <- ranking_values[!is.na(ranking_values)]
  if (length(ranking_values) == 0) return(4)

  unique_ranks <- sort(unique(ranking_values))
  if (length(unique_ranks) <= 1) {
    gsea_debug_log("All ranking values identical, using 2 decimal places", 2, DEBUG_LEVEL)
    return(2)
  }

  differences <- base::diff(unique_ranks)
  min_diff    <- min(differences[differences > 0], na.rm = TRUE)

  if (is.infinite(min_diff) || is.na(min_diff) || min_diff <= 0) {
    gsea_debug_log("Could not determine minimum difference, using 4 decimal places", 2, DEBUG_LEVEL)
    return(4)
  }

  decimal_places_needed <- max(0, -floor(log10(min_diff)) + 1)
  decimal_places        <- pmax(2, pmin(12, decimal_places_needed))

  gsea_debug_log(
    paste("Min difference:", format(min_diff, scientific = TRUE),
          "-> Using", decimal_places, "decimal places"),
    2, DEBUG_LEVEL
  )
  decimal_places
}

# ============================================================
# Text Wrapping for Pathway Labels
# ============================================================

#' Wrap long pathway names at word boundaries for improved readability
#'
#' @param text_vector Character vector of pathway names.
#' @param max_chars Integer; maximum characters per line (default 40).
#' @param max_lines Integer; maximum number of lines (default 3).
#' @return Character vector with newlines inserted.
gsea_smart_wrap_pathways <- function(text_vector, max_chars = 40, max_lines = 3) {
  sapply(text_vector, function(text) {
    if (nchar(text) <= max_chars) return(text)

    words <- strsplit(text, " ")[[1]]
    if (length(words) <= 1) return(text)

    lines        <- character()
    current_line <- ""

    for (word in words) {
      test_line <- if (nchar(current_line) == 0) word else paste(current_line, word)
      if (nchar(test_line) <= max_chars) {
        current_line <- test_line
      } else {
        if (nchar(current_line) > 0) lines <- c(lines, current_line)
        current_line <- word
        if (length(lines) >= max_lines - 1) break
      }
    }
    if (nchar(current_line) > 0) lines <- c(lines, current_line)

    if (length(lines) >= max_lines) {
      all_words  <- unlist(strsplit(paste(lines, collapse = " "), " "))
      if (length(words) > length(all_words)) {
        lines[max_lines] <- paste(substr(lines[max_lines], 1, max_chars - 3), "...")
        lines <- lines[seq_len(max_lines)]
      }
    }

    paste(lines, collapse = "\n")
  }, USE.NAMES = FALSE)
}

#' Apply smart wrapping to pathway labels in an enrichplot ggplot object
#'
#' Converts underscores to spaces, wraps labels by plot type, and applies
#' differential colouring to pathway versus gene labels in GeomTextRepel layers.
#'
#' @param plot_obj ggplot object.
#' @param original_pathways Character vector; pathway names used in showCategory.
#' @param plot_type Character; controls wrap parameters ("enrichment_map",
#'   "cnet_plot", "ridgeline", "dotplot", or "default").
#' @param pathway_label_color Color for pathway labels (default "black").
#' @param protein_label_color Color for gene/protein labels (default "#8F8F8F").
#' @param debug_log Function(message, level); logging function.
#' @return Modified ggplot object.
apply_gsea_smart_wrap_post_processing <- function(plot_obj, original_pathways,
                                                   plot_type          = "default",
                                                   pathway_label_color = "black",
                                                   protein_label_color = "#8F8F8F",
                                                   debug_log) {
  debug_log("Starting GSEA smart wrap post-processing", 1)
  debug_log(paste("Plot type:", plot_type, "| Pathways:", length(original_pathways)), 2)

  tryCatch({
    pathways_for_wrapping <- gsub("_", " ", original_pathways)

    wrap_params <- switch(plot_type,
      "enrichment_map" = list(max_chars = 25, max_lines = 2),
      "cnet_plot"      = list(max_chars = 30, max_lines = 2),
      "ridgeline"      = list(max_chars = 40, max_lines = 3),
      "dotplot"        = list(max_chars = 45, max_lines = 3),
      list(max_chars = 35, max_lines = 3)
    )

    wrapped_pathways <- gsea_smart_wrap_pathways(pathways_for_wrapping,
                                                  max_chars = wrap_params$max_chars,
                                                  max_lines = wrap_params$max_lines)
    wrapped_mapping  <- setNames(wrapped_pathways, original_pathways)
    wrapped_count    <- sum(grepl("\n", wrapped_pathways))

    debug_log(paste("Wrapped mapping created:", wrapped_count, "pathways wrapped"), 1)

    text_repel_layers <- c()
    for (i in seq_along(plot_obj$layers)) {
      layer_class <- class(plot_obj$layers[[i]]$geom)
      if ("GeomTextRepel" %in% layer_class) text_repel_layers <- c(text_repel_layers, i)
    }

    if (length(text_repel_layers) == 0) {
      debug_log("No GeomTextRepel layers found - smart wrap not applicable", 1)
      return(plot_obj)
    }

    for (layer_idx in text_repel_layers) {
      plot_built <- ggplot_build(plot_obj)
      layer_data <- plot_built$data[[layer_idx]]

      if ("label" %in% names(layer_data)) {
        original_labels <- layer_data$label
        layer_data$label <- sapply(original_labels, function(label) {
          if (label %in% names(wrapped_mapping)) wrapped_mapping[[label]] else label
        }, USE.NAMES = FALSE)
        layer_data$colour <- sapply(original_labels, function(label) {
          if (label %in% names(wrapped_mapping)) pathway_label_color else protein_label_color
        }, USE.NAMES = FALSE)

        require(ggrepel)
        new_layer <- ggrepel::geom_text_repel(
          data    = layer_data,
          mapping = aes(x = x, y = y, label = label, color = I(colour)),
          size    = if ("size" %in% names(layer_data)) layer_data$size[1] else 3.5,
          hjust   = 0.5, vjust = 0.5,
          max.overlaps = Inf,
          lineheight   = 0.9,
          segment.size  = 0.3,
          segment.alpha = 0.6,
          show.legend   = FALSE
        )
        plot_obj$layers[[layer_idx]] <- NULL
        plot_obj$layers <- append(plot_obj$layers, list(new_layer), after = layer_idx - 1)
      }
    }

    debug_log(paste("GSEA smart wrap completed:", wrapped_count, "pathways wrapped"), 1)
    plot_obj

  }, error = function(e) {
    debug_log(paste("GSEA smart wrap failed:", e$message), 1)
    plot_obj
  })
}

# ============================================================
# P-value Smart Formatting
# ============================================================

#' Determine smart formatting parameters for GSEA p-values
#'
#' @param values Numeric vector of p-values.
#' @return Named list with elements use_scientific, digits, significant.
gsea_determine_smart_pvalue_format <- function(values) {
  values <- values[!is.na(values) & is.finite(values) & values > 0]
  if (length(values) == 0) return(list(use_scientific = FALSE, digits = 3, significant = 3))

  min_val    <- min(values)
  max_val    <- max(values)
  log10_min  <- log10(min_val)
  log10_max  <- log10(max_val)

  if (min_val < 1e-10) {
    list(use_scientific = TRUE,  digits = 3, significant = 2)
  } else if (min_val < 1e-4) {
    list(use_scientific = TRUE,  digits = 3, significant = 3)
  } else if ((log10_max - log10_min) > 3) {
    list(use_scientific = TRUE,  digits = 3, significant = 3)
  } else {
    digits <- if (max_val < 0.001) 4 else if (max_val < 0.01) 3 else 2
    list(use_scientific = FALSE, digits = digits, significant = 3)
  }
}

#' Format p-values using parameters from gsea_determine_smart_pvalue_format()
#'
#' @param breaks Numeric vector of p-values to format.
#' @param format_params List from gsea_determine_smart_pvalue_format().
#' @return Character vector of formatted values.
gsea_format_pvalues_smart <- function(breaks, format_params) {
  if (format_params$use_scientific) {
    formatted <- format(breaks, scientific = TRUE, digits = format_params$significant)
    formatted <- gsub("e\\+00$", "", formatted)
    formatted <- gsub("e-0",     "e-",  formatted)
    formatted <- gsub("e\\+",    "e",   formatted)
    return(formatted)
  }
  format(breaks, nsmall = format_params$digits, digits = format_params$digits, scientific = FALSE)
}

# ============================================================
# Miscellaneous Helpers
# ============================================================

#' Validate that an abundance type string is one of the recognised types
#'
#' @param abundance_type Character; the type to validate.
#' @return Logical.
validate_abundance_type <- function(abundance_type) {
  if (is.null(abundance_type) || length(abundance_type) == 0 || abundance_type == "") {
    return(FALSE)
  }
  allowed_pattern <- paste0(
    "^(Normalized Abundance|Imputed Raw Abundance|",
    "Imputed Normalized Abundance|Imputed Batch Corrected Normalized Abundance|",
    "Imputed Batch Corrected Raw Abundance|Batch Corrected Raw Abundance|",
    "Batch Corrected Normalized Abundance|Raw Abundance)$"
  )
  grepl(allowed_pattern, abundance_type, ignore.case = FALSE)
}

#' Normalise a validation logic string to a canonical key
#'
#' @param valid_logic Character; user-facing logic string.
#' @return Character; one of "one", "total", "each".
normalize_validation_logic <- function(valid_logic) {
  if (is.null(valid_logic) || !nzchar(valid_logic)) return("one")
  vl <- tolower(trimws(gsub("[ _-]+", " ", valid_logic)))
  if (vl %in% c("in total", "total")) "total"
  else if (vl %in% c("in one group", "one group", "one")) "one"
  else "each"
}

#' Get original (non-imputed) data columns for validation when imputed data is selected
#'
#' @param data Data frame; the full data.
#' @param content_type Character; the selected content type.
#' @param metadata_definition Data frame; the metadata definition.
#' @param safe_log Function(message, level); optional logging function.
#' @return Data frame; data with imputed columns replaced by original values.
get_validation_data_for_imputed_content <- function(data, content_type,
                                                      metadata_definition,
                                                      safe_log = function(...) {}) {
  tryCatch({
    if (is.null(content_type) || is.null(metadata_definition)) return(data)
    if (!grepl("^Imputed", content_type, ignore.case = TRUE)) {
      safe_log("Non-imputed data, using current data for validation", 2)
      return(data)
    }

    original_content_type  <- gsub("^Imputed\\s+", "", content_type, ignore.case = TRUE)
    safe_log(paste("Imputed data detected. Original type:", original_content_type), 1)

    original_columns <- metadata_definition$Column[
      metadata_definition$Content == original_content_type
    ]
    original_columns <- original_columns[!is.na(original_columns)]
    existing_original_columns <- original_columns[original_columns %in% names(data)]

    if (length(existing_original_columns) == 0) {
      safe_log("No original data columns found, using current data", 2)
      return(data)
    }

    validation_data  <- data
    imputed_columns  <- metadata_definition$Column[metadata_definition$Content == content_type]
    imputed_columns  <- imputed_columns[!is.na(imputed_columns) & imputed_columns %in% names(data)]

    for (imp_col in imputed_columns) {
      potential_orig_col <- gsub("^Imputed\\s+", "", imp_col, ignore.case = TRUE)
      if (potential_orig_col %in% existing_original_columns) {
        validation_data[, imp_col] <- data[, potential_orig_col]
        safe_log(paste("Using original values from", potential_orig_col, "for validation"), 2)
      }
    }

    validation_data
  }, error = function(e) {
    safe_log(paste("Error in validation data setup:", e$message), 1)
    data
  })
}
