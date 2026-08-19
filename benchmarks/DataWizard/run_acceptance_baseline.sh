#!/usr/bin/env bash
set -euo pipefail
repetitions="${REPETITIONS:-15}"
fixtures="${1:-tests/fixtures/datawizard_upload_telemetry/generated}"
output="${2:-benchmarks/DataWizard/upload_baseline.csv}"
rm -f "$output"
for fixture in "$fixtures"/*.csv "$fixtures"/*.xlsx; do
  [[ -f "$fixture" ]] || continue
  for ((i=1; i<=repetitions; i++)); do
    Rscript benchmarks/DataWizard/benchmark_upload_lifecycle.R "$fixture" "$output"
  done
done
Rscript - "$output" <<'RS'
args <- commandArgs(trailingOnly=TRUE); x <- read.csv(args[[1]])
summary <- do.call(rbind, lapply(split(x, x$fixture), function(z) data.frame(
  fixture=z$fixture[[1]], repetitions=nrow(z),
  parse_median_ms=median(z$parse_ms), parse_p95_ms=unname(quantile(z$parse_ms,.95)),
  normalization_median_ms=median(z$normalization_ms),
  normalization_p95_ms=unname(quantile(z$normalization_ms,.95)))))
write.csv(summary, sub("[.]csv$", "_summary.csv", args[[1]]), row.names=FALSE)
print(summary, row.names=FALSE)
RS
