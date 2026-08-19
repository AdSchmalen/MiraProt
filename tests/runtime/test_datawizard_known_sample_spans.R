library(testthat)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")

known <- c("mock", "mock_IFNy", "AAV2_eGFP", "AAV2_eGFP_IFNy",
  "Capsid", "Capsid_IFNy")
pattern_rule <- data.frame(Method = "Pattern Recognition", Separators = "_",
  Invert = FALSE, stringsAsFactors = FALSE)

expect_components <- function(header, numerator, denominator, invert = FALSE) {
  pattern_rule$Invert <- invert
  expected <- list(numerator = numerator, denominator = denominator)
  expect_identical(ratio_extract(header, pattern_rule, known), expected)
  old <- options(dw_known_samples = known)
  on.exit(options(old), add = TRUE)
  expect_identical(extract_ratio_components_pattern(header, "_", invert), expected)
}

test_that("longest exact known-sample spans win in source order", {
  expect_components("mock_IFNy / mock", "mock_IFNy", "mock")
  expect_components("AAV2_eGFP_IFNy / AAV2_eGFP", "AAV2_eGFP_IFNy", "AAV2_eGFP")
  expect_components("Capsid_IFNy / Capsid", "Capsid_IFNy", "Capsid")
  expect_components("mock / mock_IFNy", "mock", "mock_IFNy")
  expect_components("mock_IFNy / mock", "mock", "mock_IFNy", invert = TRUE)
})

test_that("ambiguous, repeated, and excess spans abstain", {
  expect_null(datawizard_resolve_known_sample_ratio("mock / mock", known))
  expect_null(datawizard_resolve_known_sample_ratio(
    "mock / Capsid / AAV2_eGFP", known))
  spans <- datawizard_known_sample_spans("mock_IFNy / mock", known)
  expect_identical(spans$value, c("mock_IFNy", "mock"))
  expect_named(spans, c("value", "start", "end", "is_unique"))
})

test_that("serialized rule replay and Auto-Assign runtime use shared semantics", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(pattern_rule, path)
  replayed <- readRDS(path)
  expect_identical(ratio_extract("Capsid / Capsid_IFNy", replayed, known),
    list(numerator = "Capsid", denominator = "Capsid_IFNy"))

  old <- options(dw_known_samples = known)
  on.exit(options(old), add = TRUE)
  expect_identical(extract_ratio_components_from_rule(
    "AAV2_eGFP / AAV2_eGFP_IFNy", replayed),
    list(numerator = "AAV2_eGFP", denominator = "AAV2_eGFP_IFNy"))
})
