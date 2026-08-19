# GSEA epsilon benchmark

This benchmark isolates the `eps` choice on the workload dimensions reported by
the application: **4,177 ranked genes**, all **50 Hallmark gene sets**, and the
production GSEA settings (`minSize=10`, `maxSize=500`, `scoreType="std"`,
`nPermSimple=1000`, and BH adjustment). The ranking, seed, gene sets, and
`BiocParallel::SerialParam()` backend are fixed between arms.

The positive comparison value is `1e-10`, the documented default of
[`fgseaMultilevel()`](https://bioconductor.org/packages/release/bioc/manuals/fgsea/man/fgsea.pdf).
In fgsea, `eps` bounds the minimum p-value used during multilevel estimation;
therefore a positive value trades resolution below that bound for the potential
to stop estimation sooner.

Run from the repository root:

```sh
Rscript benchmarks/GSEA/benchmark_fgsea_epsilon.R
```

An optional first argument selects the repetition count (default: five). The
script warms both arms, alternates their execution order, records every elapsed
time in `results/timings.csv`, writes pathway-level ordering/NES/p-value/
adjusted-p-value/significance comparisons to `results/pathway_comparison.csv`,
and writes the aggregate decision to `results/summary.md`.

## Decision rule and current default

A speedup is considered material at **1.20x or greater** using median elapsed
time. Precision is accepted only when the maximum absolute NES difference is at
most `1e-6` and no pathway changes BH-adjusted significance membership at 0.05.
Both conditions must hold before an epsilon default change qualifies for review.

The checked-in application default remains `eps = 0`: no completed benchmark in
the development container demonstrated that both gates were met. Generated
results are machine-specific and intentionally ignored; attach the complete
`results/` directory when reporting a benchmark run.
