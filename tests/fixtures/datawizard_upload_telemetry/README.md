# Upload telemetry fixtures

Fixtures are generated rather than stored as large binaries. Run
`Rscript benchmarks/DataWizard/generate_fixtures.R`. The deterministic generator
creates narrow, wide, and large data with synthetic row identifiers and numeric
values in both CSV and XLSX (when `writexl` is installed).
