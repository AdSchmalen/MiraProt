library(testthat)

source("modules/Data Wizard/ratios/datawizard_ratios_validation.R", local = TRUE)

make_mapping <- function(id = "contrast-1", columns = c(
  ratio = "opaque-ratio", p_value = "opaque-p", adjusted_p_value = "opaque-adj"
)) {
  create_datawizard_contrast_mapping(
    id, columns, c("treated replicate group"), c("vehicle reference group"),
    source_revision = "primary-working:42",
    available_group_refs = c("treated replicate group", "vehicle reference group")
  )
}

test_that("opaque result headings share a ContrastId but retain variant identity", {
  mapping <- make_mapping()
  expect_length(unique(mapping$Columns$ContrastId), 1L)
  expect_identical(mapping$Columns$Content, c(
    "Abundance Ratio", "Abundance Ratio p-Value", "Abundance Ratio Adj. p-Value"
  ))
  expect_length(unique(mapping$Columns$VariantId), 3L)
  expect_identical(mapping$NumeratorRefs, "treated replicate group")
  expect_identical(mapping$DenominatorRefs, "vehicle reference group")
  expect_identical(mapping$SourceRevision, "primary-working:42")
})

test_that("normalizing generated column headings preserves a complete triplet", {
  mapping <- make_mapping(columns = c(
    ratio = " ratio output ",
    p_value = " p-value output ",
    adjusted_p_value = " adjusted p-value output "
  ))

  expect_false(mapping$Partial)
  expect_identical(mapping$MissingVariants, character())
  expect_identical(mapping$Columns$Column, c(
    "ratio output", "p-value output", "adjusted p-value output"
  ))
  expect_identical(mapping$Columns$VariantId, paste0(
    "contrast-1:", c("ratio", "p_value", "adjusted_p_value")
  ))
})

test_that("collection rejects incomplete, ambiguous, and colliding relationships", {
  expect_error(make_mapping(columns = c(ratio = "only-one")), "complete or explicitly")
  partial <- create_datawizard_contrast_mapping(
    "partial", c(ratio = "ratio-only"), "A", "B", "7", c("A", "B"), partial = TRUE,
    missing_variants = c("p_value", "adjusted_p_value"))
  expect_true(partial$Partial)
  expect_error(create_datawizard_contrast_mapping("bad", c(ratio = "x"), "", "B", "7", c("A", "B"),
    partial = TRUE, missing_variants = c("p_value", "adjusted_p_value")), "actual, non-empty")
  expect_error(create_datawizard_contrast_mapping("bad", c(ratio = "x"), "A", "A", "7", c("A", "B"),
    partial = TRUE, missing_variants = c("p_value", "adjusted_p_value")), "must not overlap")
  expect_error(create_datawizard_contrast_mapping("bad", c(ratio = "x"), "invented", "B", "7", c("A", "B"),
    partial = TRUE, missing_variants = c("p_value", "adjusted_p_value")), "actual source groups")
  expect_error(create_datawizard_contrast_mapping_collection(list(make_mapping(), make_mapping())), "unique")
  expect_error(create_datawizard_contrast_mapping_collection(list(
    make_mapping("one"), make_mapping("two", c(ratio = "opaque-ratio", p_value = "p2", adjusted_p_value = "a2")))),
    "collision")
})

test_that("versioned collection round trips without treating operands as ids", {
  collection <- create_datawizard_contrast_mapping_collection(list(make_mapping()))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(collection, path)
  restored <- readRDS(path)
  expect_silent(validate_datawizard_contrast_mapping_collection(restored))
  expect_identical(restored, collection)
  expect_identical(names(restored$mappings), "contrast-1")
  expect_false(any(c(restored$mappings[[1]]$NumeratorRefs,
                     restored$mappings[[1]]$DenominatorRefs) %in% names(restored$mappings)))
})
