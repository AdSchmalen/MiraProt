library(testthat)

source("modules/Data Wizard/datawizard_provenance.R")
source("modules/Data Wizard/ratios/datawizard_ratios_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_provenance.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R")

provenance_fixture <- function(origin, generated, sources = "source") {
  record <- datawizard_provenance_record(origin, generated, sources,
    family = paste0(origin, "-family"))
  metadata <- data.frame(Column = c(sources, generated), Content = "",
    stringsAsFactors = FALSE, check.names = FALSE)
  generated_rows <- datawizard_provenance_metadata_rows(record,
    metadata[rep(1L, length(generated)), , drop = FALSE])
  metadata[(length(sources) + 1L):nrow(metadata), names(generated_rows)] <- generated_rows
  data <- as.data.frame(stats::setNames(replicate(nrow(metadata), logical(), simplify = FALSE),
    metadata$Column), check.names = FALSE)
  list(metadata = metadata, data = data)
}

test_that("complete generated Data Wizard families resolve for every provenance origin", {
  fixtures <- list(
    ratio = provenance_fixture("ratio", paste0("A_vs_B", c("_Abundance Ratio",
      "_Abundance Ratio p-Value", "_Abundance Ratio Adj. p-Value"))),
    imputation = provenance_fixture("imputation", "Imputed intensity"),
    batch_correction = provenance_fixture("batch_correction", "Batch Corrected intensity"),
    basemean = provenance_fixture("basemean", "log2_Basemean"),
    merge = provenance_fixture("merge", "Merged annotation")
  )
  for (fixture in fixtures) {
    result <- auto_regex_resolve_provenance(fixture$metadata, fixture$data)
    expect_true(all(result$Outcome[-1L] == "resolved"))
  }
})

test_that("mixed generated operations resolve together while partial provenance abstains", {
  operations <- list(
    provenance_fixture("imputation", "Imputed intensity"),
    provenance_fixture("batch_correction", "Batch Corrected intensity"),
    provenance_fixture("basemean", "log2_Basemean"),
    provenance_fixture("merge", "Merged annotation")
  )
  metadata <- do.call(rbind, lapply(seq_along(operations), function(i) {
    x <- operations[[i]]$metadata
    x$Column <- paste0(i, "_", x$Column)
    source_rows <- seq_len(nrow(x)) == 1L
    generated_rows <- !source_rows
    x[["Provenance Source Columns"]][generated_rows] <- x$Column[source_rows]
    x
  }))
  data <- as.data.frame(stats::setNames(
    replicate(nrow(metadata), logical(), simplify = FALSE), metadata$Column
  ), check.names = FALSE)
  resolved <- auto_regex_resolve_provenance(metadata, data)
  expect_identical(sum(resolved$Outcome == "resolved"), 4L)

  partial <- operations[[1L]]
  partial$metadata[["Provenance Source Columns"]][2L] <- "missing-column"
  unresolved <- auto_regex_resolve_provenance(partial$metadata, partial$data)
  expect_identical(unresolved$Outcome[2L], "missing source")
})

test_that("workbook provenance does not evaluate or mix active session state", {
  fixture <- provenance_fixture("imputation", "Imputed intensity")
  descriptor <- auto_regex_source_provenance("excel", fixture$metadata, NULL,
    function() stop("session state leaked"))
  expect_identical(descriptor$source, "workbook")
  expect_null(descriptor$contrast_mapping_collection)
  expect_setequal(names(descriptor$data), fixture$metadata$Column)
  prepared <- auto_regex_prepare_provenance(fixture$metadata, descriptor)
  expect_identical(prepared$diagnostics$Outcome[2L], "resolved")
})

test_that("workbook mode rejects legacy exact-name authority and exposes broken lineage", {
  legacy <- data.frame(Column = c("source", "Imputed intensity"), Content = "",
    stringsAsFactors = FALSE)
  descriptor <- auto_regex_source_provenance("excel", legacy, NULL,
    list(configurations = list(imputation = list())))
  expect_false(any(auto_regex_prepare_provenance(legacy, descriptor)$diagnostics$Outcome == "resolved"))

  fixture <- provenance_fixture("merge", "Merged annotation")
  fixture$metadata[["Provenance Source Columns"]][2L] <- "missing"
  descriptor <- auto_regex_source_provenance("excel", fixture$metadata, NULL)
  result <- auto_regex_prepare_provenance(fixture$metadata, descriptor)
  expect_identical(result$diagnostics$Outcome[2L], "missing source")
  expect_length(result$authoritative_rows, 0L)
})

test_that("persisted ratio triplets receive one deterministic synthetic contrast", {
  fixture <- provenance_fixture("ratio", paste0("A_vs_B", c("_Abundance Ratio",
    "_Abundance Ratio p-Value", "_Abundance Ratio Adj. p-Value")))
  descriptor <- auto_regex_source_provenance("excel", fixture$metadata, NULL)
  result <- auto_regex_prepare_provenance(fixture$metadata, descriptor)
  rows <- 2:4
  expect_identical(result$metadata$Numerator[rows],
    rep("datawizard_contrast_1_numerator", 3L))
  expect_identical(result$metadata$Denominator[rows],
    rep("datawizard_contrast_1_denominator", 3L))
  expect_length(unique(sub(":.*$", "", result$metadata$VariantId[rows])), 1L)
})

test_that("active contrast mappings are scoped to their exact source revision", {
  fixture <- provenance_fixture("ratio", paste0("arbitrary dashboard text", c(
    "_Abundance Ratio", "_Abundance Ratio p-Value",
    "_Abundance Ratio Adj. p-Value")))
  columns <- stats::setNames(fixture$metadata$Column[2:4],
    c("ratio", "p_value", "adjusted_p_value"))
  mapping <- create_datawizard_contrast_mapping("known-contrast", columns,
    "treated", "control", "17", c("treated", "control"))
  collection <- create_datawizard_contrast_mapping_collection(list(mapping))

  stale <- auto_regex_source_provenance("active", fixture$metadata, fixture$data,
    list(source_revision = "18", contrast_mapping_collection = collection))
  expect_null(stale$contrast_mapping_collection)
  stale_result <- auto_regex_prepare_provenance(fixture$metadata, stale)
  expect_false(any(grepl("treated|control|arbitrary dashboard text",
    c(stale_result$metadata$Numerator[2:4], stale_result$metadata$Denominator[2:4]))))
  expect_length(unique(sub(":.*$", "", stale_result$metadata$VariantId[2:4])), 1L)

  current <- auto_regex_source_provenance("active", fixture$metadata, fixture$data,
    list(source_revision = "17", contrast_mapping_collection = collection))
  current_result <- auto_regex_prepare_provenance(fixture$metadata, current)
  expect_identical(current_result$metadata$Numerator[2:4], rep("treated", 3L))
  expect_identical(current_result$metadata$Denominator[2:4], rep("control", 3L))
  expect_identical(unique(sub(":.*$", "", current_result$metadata$VariantId[2:4])),
    "known-contrast")
})
