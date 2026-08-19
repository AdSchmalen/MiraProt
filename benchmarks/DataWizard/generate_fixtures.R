#!/usr/bin/env Rscript
# Deterministic, synthetic fixtures: no production data or identifying values.
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "tests/fixtures/datawizard_upload_telemetry/generated"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(20260803)
shapes <- list(narrow = c(1000L, 8L), wide = c(250L, 300L), large = c(20000L, 60L))
make_frame <- function(nr, nc) {
  values <- matrix(round(runif(nr * nc), 6), nrow = nr, ncol = nc)
  data.frame(RowID = sprintf("row_%06d", seq_len(nr)), values,
             check.names = FALSE, stringsAsFactors = FALSE)
}
for (name in names(shapes)) {
  shape <- shapes[[name]]
  fixture <- make_frame(shape[[1]], shape[[2]] - 1L)
  utils::write.csv(fixture, file.path(out, paste0(name, ".csv")), row.names = FALSE)
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(list(Data = fixture, SmallSheet = head(fixture, 25L)),
                        file.path(out, paste0(name, ".xlsx")))
  } else {
    warning("writexl is unavailable; CSV fixtures were generated but XLSX fixtures were skipped")
  }
}
