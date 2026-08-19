#!/usr/bin/env Rscript

# Compare the production epsilon with fgsea's documented positive default on a
# deterministic workload matching the dimensions seen in the application log.
suppressPackageStartupMessages({
  library(BiocParallel)
  library(fgsea)
})

args <- commandArgs(trailingOnly = TRUE)
iterations <- if (length(args)) as.integer(args[[1L]]) else 5L
if (is.na(iterations) || iterations < 1L) stop("iterations must be a positive integer")

gmt_path <- file.path("GSEA", "h.all.v2025.1.Hs.symbols.gmt")
output_dir <- file.path("benchmarks", "GSEA", "results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_gmt <- function(path) {
  rows <- strsplit(readLines(path, warn = FALSE), "\t", fixed = TRUE)
  stats::setNames(lapply(rows, function(x) unique(x[-c(1L, 2L)])),
                  vapply(rows, `[[`, character(1L), 1L))
}

pathways <- read_gmt(gmt_path)
stopifnot(length(pathways) == 50L)

# Include every Hallmark symbol before adding deterministic non-members. This
# preserves realistic set overlap while fixing the ranked vector at 4,177.
hallmark_genes <- unique(unlist(pathways, use.names = FALSE))
if (length(hallmark_genes) > 4177L) stop("Hallmark universe exceeds benchmark size")
genes <- c(hallmark_genes,
           sprintf("BENCHMARK_FILLER_%04d", seq_len(4177L - length(hallmark_genes))))
set.seed(20260727L)
ranks <- stats::rnorm(length(genes))
names(ranks) <- genes
# Add fixed biological-looking signal so precision is exercised for enriched
# pathways rather than benchmarking only null pathways.
for (i in seq_len(5L)) ranks[pathways[[i]]] <- ranks[pathways[[i]]] + (3.0 - i / 4)
ranks <- sort(ranks, decreasing = TRUE)
stopifnot(length(ranks) == 4177L, !anyDuplicated(names(ranks)))

parameters <- list(
  minSize = 10L,
  maxSize = 500L,
  scoreType = "std",
  nPermSimple = 1000L
)
backend <- BiocParallel::SerialParam() # deliberately identical for both arms
epsilons <- c(exact = 0, positive = 1e-10)

run_one <- function(eps) {
  set.seed(20260727L)
  started <- proc.time()[["elapsed"]]
  result <- fgsea::fgseaMultilevel(
    pathways = pathways, stats = ranks,
    minSize = parameters$minSize, maxSize = parameters$maxSize,
    scoreType = parameters$scoreType, nPermSimple = parameters$nPermSimple,
    eps = eps, BPPARAM = backend
  )
  elapsed <- proc.time()[["elapsed"]] - started
  result <- as.data.frame(result)
  result <- result[order(result$pval, -abs(result$NES), result$pathway), ]
  result$order <- seq_len(nrow(result))
  list(result = result, elapsed = elapsed)
}

# Warm both code paths, then alternate order to reduce cache/order bias.
invisible(run_one(epsilons[["exact"]]))
invisible(run_one(epsilons[["positive"]]))
runs <- vector("list", iterations * 2L)
k <- 1L
for (i in seq_len(iterations)) {
  arm_order <- if (i %% 2L) names(epsilons) else rev(names(epsilons))
  for (arm in arm_order) {
    value <- run_one(epsilons[[arm]])
    runs[[k]] <- data.frame(iteration = i, arm = arm, eps = epsilons[[arm]],
                            elapsed_seconds = value$elapsed)
    attr(runs[[k]], "result") <- value$result
    k <- k + 1L
  }
}
timings <- do.call(rbind, runs)
write.csv(timings, file.path(output_dir, "timings.csv"), row.names = FALSE)

# Compare paired final runs. Identical seeds make each arm reproducible while
# still retaining all timing repetitions above.
exact <- attr(runs[[max(which(timings$arm == "exact"))]], "result")
positive <- attr(runs[[max(which(timings$arm == "positive"))]], "result")
exact <- exact[, c("pathway", "order", "NES", "pval", "padj")]
positive <- positive[, c("pathway", "order", "NES", "pval", "padj")]
comparison <- merge(exact, positive, by = "pathway", suffixes = c("_eps0", "_eps1e10"))
comparison$order_delta <- comparison$order_eps1e10 - comparison$order_eps0
comparison$NES_abs_delta <- abs(comparison$NES_eps1e10 - comparison$NES_eps0)
comparison$pval_abs_delta <- abs(comparison$pval_eps1e10 - comparison$pval_eps0)
comparison$padj_abs_delta <- abs(comparison$padj_eps1e10 - comparison$padj_eps0)
comparison$significant_eps0 <- comparison$padj_eps0 < 0.05
comparison$significant_eps1e10 <- comparison$padj_eps1e10 < 0.05
comparison$significance_changed <- comparison$significant_eps0 != comparison$significant_eps1e10
write.csv(comparison, file.path(output_dir, "pathway_comparison.csv"), row.names = FALSE)

medians <- tapply(timings$elapsed_seconds, timings$arm, median)
speedup <- unname(medians[["exact"]] / medians[["positive"]])
material <- is.finite(speedup) && speedup >= 1.20
precision_ok <- max(comparison$NES_abs_delta, na.rm = TRUE) <= 1e-6 &&
  sum(comparison$significance_changed, na.rm = TRUE) == 0L
summary_lines <- c(
  "# fgsea epsilon benchmark result", "",
  sprintf("- Date: %s", Sys.Date()),
  sprintf("- R: %s", R.version.string),
  sprintf("- fgsea: %s", as.character(packageVersion("fgsea"))),
  sprintf("- Workload: %d ranked genes; %d Hallmark gene sets", length(ranks), length(pathways)),
  sprintf("- Fixed parameters: seed=20260727; backend=SerialParam; minSize=%d; maxSize=%d; scoreType=%s; nPermSimple=%d",
          parameters$minSize, parameters$maxSize, parameters$scoreType, parameters$nPermSimple),
  sprintf("- Median elapsed, eps=0: %.6f seconds", medians[["exact"]]),
  sprintf("- Median elapsed, eps=1e-10: %.6f seconds", medians[["positive"]]),
  sprintf("- Speedup (eps=0 / eps=1e-10): %.3fx", speedup),
  sprintf("- Exact pathway-order matches: %d/%d", sum(comparison$order_delta == 0L), nrow(comparison)),
  sprintf("- Spearman pathway-order correlation: %.12f", stats::cor(comparison$order_eps0, comparison$order_eps1e10, method = "spearman")),
  sprintf("- Maximum absolute NES difference: %.12g", max(comparison$NES_abs_delta, na.rm = TRUE)),
  sprintf("- Maximum absolute p-value difference: %.12g", max(comparison$pval_abs_delta, na.rm = TRUE)),
  sprintf("- Maximum absolute adjusted-p-value difference: %.12g", max(comparison$padj_abs_delta, na.rm = TRUE)),
  sprintf("- Significance-membership changes (BH adjusted p < 0.05): %d", sum(comparison$significance_changed, na.rm = TRUE)),
  sprintf("- Decision: %s", if (material && precision_ok) "epsilon change qualifies for review" else "retain eps=0")
)
writeLines(summary_lines, file.path(output_dir, "summary.md"))
cat(paste(summary_lines, collapse = "\n"), "\n")
