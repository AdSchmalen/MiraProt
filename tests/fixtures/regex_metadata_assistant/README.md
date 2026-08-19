# Regex Metadata Assistant golden Excel source sheets

The core CSV files in this directory are the text-only, deterministic source sheets for the representative Excel workbook. Additional focused CSV fixtures exercise runtime inference and naming without storing binary workbooks in Git. The standalone self-test reads the workbook source sheets, creates a temporary `.xlsx` workbook with `openxlsx`, and then reads that workbook through `readxl`, exactly as the interactive upload boundary does.

| Source file | Generated Excel sheet | Coverage |
|---|---|---|
| `content.csv` | `content` | content-rule inference and transformation preservation |
| `condition.csv` | `condition` | sample-bearing inference and non-applicable content types |
| `ratio.csv` | `ratio` | reliable one-row ratio inference |
| `grouped_workflows.csv` | focused runtime fixture | unchanged single-rule replay plus Merged-only, Data-Wizard-only, and mixed ratio-header families |
| `scr528_structural_conditions.csv` | focused runtime fixture | SCR528 Proteome Discoverer headers with six underscore-bearing conditions, three replicates, three sample-bearing content labels, and a vocabulary-independent renamed scenario |
| `quantity_complete_conditions.csv` | sample-name fixture | 18 representative Quantity headers paired with their complete, underscore-preserving conditions |
| `quantity_*.csv` structural variants | sample-name fixtures | Renamed affixes and conditions, mixed identifiers, missing bracket indices, and unequal replicate counts prove vocabulary-independent naming |

The authoritative expected result is executable: `run_self_tests()` in `Regex_Metadata_Assistant.R` generates and reads all three sheets, runs the current inference functions, and asserts transformation, applicability, replay, reliability, and row-count outcomes. The migration baseline records the same expectations and exact source references.

Run from the repository root:

```sh
MIRAPROT_SELF_TEST=1 Rscript Regex_Metadata_Assistant.R
```

A behavior change is intentional only when the CSV sources, assertions, migration baseline, and relevant implementation change together.

## Complete anonymized 110-column regression workbook

`full_110_case.csv` is the annotated, anonymized source metadata for the complete
reported workbook shape, and `full_110_expected.csv` is its separate replay
oracle (`Column`, `Content`, `Options`, `Numerator`, `Denominator`,
`Transformation`, and `Sample`). Both fixtures deliberately preserve row order.
They contain the four `PG.*` protein fields; 18 rows each for Raw Abundance,
Found in Sample, and Found in File; all 15 pairwise comparisons in each of the
`Merged_Ratio_*`, `Merged_Pvalue_*`, and `Merged_Qvalue_*` families; the
canonical `Row Index`; and two unreferenced Data-Wizard rows for each ratio
content type. The anonymized conditions retain the important prefix overlaps
`mock`/`mock_IFNy`, `Capsid`/`Capsid_IFNy`, and
`AAV2_eGFP`/`AAV2_eGFP_IFNy`.

The grouped-workflow acceptance test infers a complete rule contract, replays
content, condition, and ratio rules in application order, then compares every
field and all 110 ordered rows with the expected fixture. It additionally
asserts the 45 referenced Merged ratios, the six unreferenced Data-Wizard
ratios, Identifier isolation, and the protein-description assignment.
