# GSEA_module_parallelization.R
#
# Purpose:
#   Centralizes all CPU core registration, parallel backend setup, lifecycle
#   management, and safe release of parallelization resources for the GSEA module.
#
# Architecture:
#   This file is sourced by GSEA_module.R and provides self-contained parallel
#   management for clusterProfiler::GSEA(). It has no dependency on Shiny
#   reactives and can be unit-tested independently.
#
# Structure:
#   1. Safe core detection and allocation
#   2. BiocParallel backend creation and teardown
#   3. Sequential fallback notification helper
#   4. Main GSEA analysis entry point (run_gsea_analysis)
#
# Developer notes:
#   - All logging uses gsea_debug_log(message, level, DEBUG_LEVEL) from
#     GSEA_module_logic.R. High-level entry points also accept an optional
#     debug_log function parameter for integration with the server's logger.
#   - Worker lifecycle (create -> use -> stop) is handled inside
#     run_gsea_analysis() via on.exit() to guarantee cleanup even on error.
#   - Parallelization is conservative: at most 4 workers, at most 1/3 of
#     detected cores, always leaving 3 cores free for the Shiny process.
#   - BiocParallel SnowParam with SOCK type is used for cross-platform safety.
#   - If parallel setup or execution fails, the code falls back to sequential
#     mode transparently.

# ============================================================
# Safe Core Detection
# ============================================================

# Thin adapters keep package calls replaceable in unit tests without starting
# real workers or running enrichment permutations.
gsea_detect_cores <- function() parallel::detectCores()
gsea_biocparallel_available <- function() requireNamespace("BiocParallel", quietly = TRUE)
gsea_bp_serial <- function() BiocParallel::SerialParam()
gsea_bp_snow <- function(...) BiocParallel::SnowParam(...)
gsea_bp_start <- function(bp) BiocParallel::bpstart(bp)
gsea_bp_is_up <- function(bp) BiocParallel::bpisup(bp)
gsea_bp_workers <- function(bp) BiocParallel::bpworkers(bp)
gsea_bp_stop <- function(bp) BiocParallel::bpstop(bp)
gsea_read_term2gene <- function(path) clusterProfiler::read.gmt(path)
gsea_execute <- function(...) clusterProfiler::GSEA(...)
gsea_is_small_job <- function(gene_list, gene_set_count, num_permutations) {
  length(gene_list) < 1000 || gene_set_count < 25 || num_permutations < 500
}

#' Determine a safe number of parallel workers for GSEA
#'
#' Uses a conservative allocation policy: at most 4 workers, at most 1/3 of
#' detected cores, leaving at least 3 cores free for the Shiny process.
#'
#' @param debug_level Integer; logging verbosity.
#' @param requested_cores Optional integer; user-requested worker count. The
#'   actual allocation is the minimum of the computed safe target and this value.
#' @return Integer; number of workers to use (>= 1).
gsea_safe_cores <- function(debug_level = 0, requested_cores = NULL) {
  detected <- tryCatch(gsea_detect_cores(), error = function(e) 1L)
  if (is.na(detected) || detected < 1L) {
    gsea_debug_log("Core detection failed, using sequential", 2, debug_level)
    return(1L)
  }

  target <- max(1L, min(
    floor(detected / 3L),
    detected - 3L,
    4L
  ))

  req_cores <- suppressWarnings(as.integer(requested_cores))
  if (length(req_cores) == 1L && !is.na(req_cores) && req_cores >= 1L) {
    target <- min(target, req_cores)
  }

  gsea_debug_log(
    sprintf("Core detection: detected=%d, target=%d, OS=%s", detected, target, .Platform$OS.type),
    2, debug_level
  )

  as.integer(target)
}

# ============================================================
# BiocParallel Backend Creation
# ============================================================

#' Create a BiocParallel parameter object for GSEA
#'
#' The returned backend is already running. Metadata distinguishes the requested
#' worker count from the effective count reported by BiocParallel.
#'
#' @param debug_level Integer; logging verbosity.
#' @param requested_cores Optional integer; forwarded to gsea_safe_cores().
#' @return Named list with elements `bp` and `workers`.
gsea_get_bpparam <- function(debug_level = 0, requested_cores = NULL) {
  requested_workers <- gsea_safe_cores(debug_level, requested_cores)
  t0 <- proc.time()[3]

  if (!gsea_biocparallel_available()) {
    stop("BiocParallel is required to configure GSEA execution")
  }

  if (requested_workers <= 1L) {
    gsea_debug_log("BPPARAM: sequential (workers=1)", 2, debug_level)
    bp <- gsea_bp_serial()
    return(list(
      bp = bp, workers = 1L, requested_workers = 1L,
      effective_workers = 1L, execution_mode = "sequential",
      backend_class = class(bp)[1L], setup_duration = proc.time()[3] - t0
    ))
  }

  bp <- NULL
  candidate <- NULL
  setup_error <- NULL
  bp <- tryCatch({
    candidate <- gsea_bp_snow(
      workers    = requested_workers,
      type       = "SOCK",
      stop.on.error = FALSE,
      timeout    = 600
    )
    candidate <- gsea_bp_start(candidate)
    if (!isTRUE(gsea_bp_is_up(candidate))) {
      stop("SnowParam backend did not enter the running state")
    }
    candidate
  }, error = function(e) {
    setup_error <<- conditionMessage(e)
    gsea_debug_log(paste("BiocParallel setup failed:", e$message), 1, debug_level)
    if (!is.null(candidate) && isTRUE(tryCatch(gsea_bp_is_up(candidate), error = function(...) FALSE))) {
      try(gsea_bp_stop(candidate), silent = TRUE)
    }
    NULL
  })

  if (!is.null(bp)) {
    setup_duration <- proc.time()[3] - t0
    effective_workers <- as.integer(gsea_bp_workers(bp))
    backend_class <- class(bp)[1L]
    gsea_debug_log(
      sprintf("BPPARAM: %s(requested=%d, effective=%d) started in %dms (OS=%s)",
              backend_class, requested_workers, effective_workers,
              round(setup_duration * 1000), .Platform$OS.type),
      2, debug_level
    )
    return(list(
      bp = bp, workers = effective_workers,
      requested_workers = as.integer(requested_workers),
      effective_workers = effective_workers, execution_mode = "parallel",
      backend_class = backend_class, setup_duration = setup_duration
    ))
  }

  serial_bp <- gsea_bp_serial()
  list(
    bp = serial_bp, workers = 1L,
    requested_workers = as.integer(requested_workers), effective_workers = 1L,
    execution_mode = "sequential", backend_class = class(serial_bp)[1L],
    setup_duration = proc.time()[3] - t0,
    fallback_occurred = TRUE, fallback_reason = setup_error
  )
}

# ============================================================
# Sequential Fallback Notification
# ============================================================

#' Notify the user that GSEA is running in sequential mode
#'
#' Shows a Shiny notification if a reactive domain is active; otherwise logs
#' the message to the console.
#'
#' @param message Character; message to display.
#' @param debug_level Integer; logging verbosity.
#' @return Invisibly TRUE if a notification was shown, FALSE otherwise.
gsea_notify_sequential_fallback <- function(message, debug_level = 0) {
  if (requireNamespace("shiny", quietly = TRUE)) {
    domain <- shiny::getDefaultReactiveDomain()
    if (!is.null(domain)) {
      shiny::showNotification(message, type = "warning", duration = 5)
      gsea_debug_log("Sequential fallback notification shown", 2, debug_level)
      return(invisible(TRUE))
    }
  }
  gsea_debug_log(paste("Sequential fallback (no UI context):", message), 1, debug_level)
  invisible(FALSE)
}

# ============================================================
# Main GSEA Analysis Entry Point
# ============================================================

#' Run GSEA analysis with automatic parallelization and fallback
#'
#' Orchestrates the complete GSEA call including parallel backend creation,
#' execution, and guaranteed worker cleanup via on.exit().
#'
#' Parallel execution is attempted first. If it fails with a cluster/socket
#' error, the function retries transparently in sequential mode. Any other
#' error causes a NULL return.
#'
#' The number of workers actually used is stored as attribute
#' "gsea_workers_used" on the returned result object.
#'
#' @param gene_list Named numeric vector; ranking metric, sorted descending.
#' @param gene_set_file Character; path to a .gmt file.
#' @param num_permutations Integer; number of permutations for fgsea.
#' @param p_value_cutoff Numeric; p-value threshold passed to GSEA().
#' @param DEBUG_LEVEL Integer; logging verbosity.
#' @param requested_cores Optional integer; forwarded to gsea_get_bpparam().
#' @param random_seed Integer seed applied immediately before each GSEA attempt.
#' @param debug_log Optional function(message, level); if provided, used for
#'   high-level log messages instead of the module-level gsea_debug_log.
#' @return A gseaResult S4 object with attribute "gsea_workers_used", or NULL.
run_gsea_analysis <- function(gene_list,
                               gene_set_file,
                               num_permutations  = 10000,
                               p_value_cutoff    = 0.05,
                               DEBUG_LEVEL       = 1,
                               requested_cores   = NULL,
                               random_seed       = 12345,
                               debug_log         = NULL) {

  log <- if (is.function(debug_log)) {
    debug_log
  } else {
    function(msg, lvl = 1) gsea_debug_log(msg, lvl, DEBUG_LEVEL)
  }

  log("Starting clusterProfiler::GSEA execution", 1)

  if (is.null(gene_list) || length(gene_list) == 0) {
    log("Empty gene list provided", 1)
    return(NULL)
  }
  if (!file.exists(gene_set_file)) {
    log("Gene set file not found", 1)
    return(NULL)
  }
  if (any(duplicated(names(gene_list)))) {
    log("Removing duplicate gene names", 2)
    gene_list <- gene_list[!duplicated(names(gene_list))]
  }

  gene_list  <- sort(gene_list, decreasing = TRUE)
  score_type <- if (all(gene_list > 0)) "pos" else if (all(gene_list < 0)) "neg" else "std"
  log(paste("Score type:", score_type), 2)

  gene_sets <- tryCatch({
    gsea_read_term2gene(gene_set_file)
  }, error = function(e) {
    log(paste("Error reading gene set file:", e$message), 1)
    NULL
  })

  if (is.null(gene_sets) || nrow(gene_sets) == 0) {
    log("Empty or invalid gene set file", 1)
    return(NULL)
  }

  gene_set_count <- length(unique(gene_sets$term))
  log(paste("Loaded", gene_set_count, "gene sets"), 2)

  small_job <- gsea_is_small_job(gene_list, gene_set_count, num_permutations)

  if (small_job) {
    log("GSEA small workload detected; using sequential mode to avoid worker startup overhead", 1)
    t_setup <- proc.time()[3]
    serial_bp <- gsea_bp_serial()
    bp_info <- list(
      bp = serial_bp, workers = 1L, requested_workers = 1L,
      effective_workers = 1L, execution_mode = "sequential",
      backend_class = class(serial_bp)[1L],
      setup_duration = proc.time()[3] - t_setup,
      fallback_occurred = FALSE, fallback_reason = NA_character_
    )
  } else {
    bp_info <- gsea_get_bpparam(DEBUG_LEVEL, requested_cores = requested_cores)
  }

  bp         <- bp_info$bp
  requested_workers <- bp_info$requested_workers
  effective_workers <- bp_info$effective_workers
  execution_mode <- bp_info$execution_mode
  backend_class <- bp_info$backend_class
  setup_duration <- bp_info$setup_duration
  fallback_occurred <- isTRUE(bp_info$fallback_occurred)
  fallback_reason <- if (is.null(bp_info$fallback_reason)) NA_character_ else bp_info$fallback_reason
  mode_label <- if (identical(execution_mode, "parallel")) {
    paste0("parallel (", effective_workers, " workers)")
  } else {
    "sequential"
  }
  fallback_notified <- FALSE

  if (fallback_occurred) {
    gsea_notify_sequential_fallback("GSEA is running in sequential mode", DEBUG_LEVEL)
    fallback_notified <- TRUE
  }

  on.exit({
    if (!is.null(bp) && gsea_biocparallel_available()) {
      tryCatch({
        gsea_bp_stop(bp)
        log("BPPARAM stopped via on.exit", 2)
      }, error = function(e) {
        log(paste("BPPARAM stop warning:", e$message), 2)
      })
    }
  }, add = TRUE)

  log(sprintf("GSEA starting: %d genes, %d gene sets, nPermSimple=%d, %s",
              length(gene_list), length(unique(gene_sets$term)),
              num_permutations, mode_label), 1)

  t_gsea   <- proc.time()[3]
  gsea_res <- tryCatch({
    set.seed(as.integer(random_seed))
    gsea_execute(
      geneList       = gene_list,
      TERM2GENE      = gene_sets,
      verbose        = FALSE,
      seed           = TRUE,
      by             = "fgsea",
      scoreType      = score_type,
      nPermSimple    = num_permutations,
      eps            = 0,
      pvalueCutoff   = p_value_cutoff,
      pAdjustMethod  = "BH",
      BPPARAM        = bp
    )
  }, error = function(e) {
    log(paste("GSEA error:", e$message), 1)
    if (identical(execution_mode, "parallel") &&
        grepl("cluster|parallel|socket", e$message, ignore.case = TRUE)) {
      log("Parallel execution failed, retrying in sequential mode", 1)
      if (!fallback_notified) {
        gsea_notify_sequential_fallback("GSEA is running in sequential mode", DEBUG_LEVEL)
      }
      fallback_reason <<- conditionMessage(e)
      tryCatch({
        fallback_bp <- gsea_bp_serial()
        set.seed(as.integer(random_seed))
        fallback_result <- gsea_execute(
          geneList      = gene_list,
          TERM2GENE     = gene_sets,
          verbose       = FALSE,
          seed          = TRUE,
          by            = "fgsea",
          scoreType     = score_type,
          nPermSimple   = num_permutations,
          eps           = 0,
          pvalueCutoff  = p_value_cutoff,
          pAdjustMethod = "BH",
          BPPARAM       = fallback_bp
        )
        fallback_occurred <<- TRUE
        execution_mode <<- "sequential"
        effective_workers <<- 1L
        backend_class <<- class(fallback_bp)[1L]
        fallback_result
      }, error = function(e2) {
        log(paste("Sequential fallback also failed:", e2$message), 1)
        NULL
      })
    } else {
      NULL
    }
  })

  gsea_elapsed <- round(proc.time()[3] - t_gsea, 1)

  if (is.null(gsea_res)) {
    log(sprintf("GSEA returned NULL after %.1fs", gsea_elapsed), 1)
    return(NULL)
  }

  sig_count <- nrow(as.data.frame(gsea_res))
  log(sprintf("GSEA completed: %.1fs, %d significant terms, mode=%s",
              gsea_elapsed, sig_count,
              if (identical(execution_mode, "parallel")) {
                paste0("parallel (", effective_workers, " workers)")
              } else "sequential"), 1)

  attr(gsea_res, "gsea_workers_used") <- as.integer(effective_workers)
  attr(gsea_res, "gsea_execution_metadata") <- list(
    requested_workers = as.integer(requested_workers),
    effective_workers = as.integer(effective_workers),
    execution_mode = execution_mode,
    backend_class = backend_class,
    nPermSimple = as.integer(num_permutations),
    random_seed = as.integer(random_seed),
    setup_start_duration_seconds = as.numeric(setup_duration),
    gsea_duration_seconds = as.numeric(gsea_elapsed),
    fallback_occurred = fallback_occurred,
    fallback_reason = fallback_reason
  )
  gsea_res
}
