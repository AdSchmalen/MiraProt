# Data Wizard upload acceptance baseline

This harness measures deterministic **narrow** (1,000 x 8), **wide** (250 x 300),
and **large** (20,000 x 60) synthetic CSV and XLSX uploads. It reports median and
95th-percentile parse and normalization time rather than treating one upload as a
baseline. Each repetition runs in a fresh R process, approximating a fresh Shiny
session and preventing loader/session caches from making later samples misleading.

```bash
Rscript benchmarks/DataWizard/generate_fixtures.R
REPETITIONS=15 benchmarks/DataWizard/run_acceptance_baseline.sh
```

Commit baseline output only when it was collected on the designated acceptance
host; compare like-for-like host, R, and package versions. Investigate regressions
in either median or p95 independently. The runtime telemetry additionally covers
reset, transactions, cache insertion, metadata creation, revision release, table
snapshot preparation, and client readiness. Do not tune the existing debounces or
synchronization/echo guards until those markers demonstrate that a guard is idle
delay rather than burst protection.
