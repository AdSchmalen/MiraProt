#!/usr/bin/env Rscript

# Load the canonical sample-bearing content definition before the Auto-Assign
# helpers that delegate to it.
source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")

stopifnot(
  identical(sample_name_needed(NULL), TRUE),
  identical(sample_name_needed(character()), TRUE),
  identical(
    sample_name_needed(c(NA_character_, "", "  \t\n", "sample 1")),
    c(TRUE, TRUE, TRUE, FALSE)
  )
)

content <- c("Raw Abundance", " Found in Sample ", "Abundance Ratio", NA_character_)
stopifnot(identical(sample_name_eligible(content), is_sample_bearing_content(content)))

# Inputs are resolved vectors and outputs are ordinary logical values.
stopifnot(
  is.logical(sample_name_needed("sample 1")),
  is.logical(sample_name_eligible("Raw Abundance"))
)
