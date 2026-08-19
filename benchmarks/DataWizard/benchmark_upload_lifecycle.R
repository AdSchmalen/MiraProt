#!/usr/bin/env Rscript
# Run ONE repetition in a fresh R process. The shell driver below starts a new
# process per repetition so package caches and Shiny session state are not reused.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("usage: benchmark_upload_lifecycle.R FIXTURE OUTPUT_CSV")
fixture <- args[[1]]; output <- args[[2]]
ext <- tolower(tools::file_ext(fixture))
t0 <- unname(proc.time()[["elapsed"]])
if (ext == "csv") {
  data <- utils::read.csv(fixture, check.names = FALSE)
} else if (ext == "xlsx") {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required")
  sheets <- readxl::excel_sheets(fixture)
  data <- as.data.frame(readxl::read_excel(fixture, sheet = sheets[[1]], .name_repair = "minimal"), check.names = FALSE)
} else stop("unsupported fixture")
parse_ms <- 1000 * (unname(proc.time()[["elapsed"]]) - t0)
t1 <- unname(proc.time()[["elapsed"]])
data <- data.frame(`Row Index` = seq_len(nrow(data)), data, check.names = FALSE)
normalization_ms <- 1000 * (unname(proc.time()[["elapsed"]]) - t1)
row <- data.frame(fixture = basename(fixture), format = ext, rows = nrow(data), columns = ncol(data),
                  object_mb = as.numeric(object.size(data)) / 1024^2,
                  parse_ms = parse_ms, normalization_ms = normalization_ms)
utils::write.table(row, output, sep = ",", row.names = FALSE,
                   col.names = !file.exists(output), append = file.exists(output))
