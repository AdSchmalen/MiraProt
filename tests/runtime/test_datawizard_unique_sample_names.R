#!/usr/bin/env Rscript

source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")

assert_same <- function(actual, expected) stopifnot(identical(unname(actual), expected))

# Private lexical helpers preserve every source character and report positions
# from both ends of the field sequence.
lexed <- .tokenize_source_headers(c("Run-01 :: mock IFN", "A,B"), c(NA, ","))
stopifnot(is.list(lexed), all(vapply(lexed, is.data.frame, logical(1))))
stopifnot(identical(paste0(lexed[[1]]$text, collapse = ""), "Run-01 :: mock IFN"))
stopifnot(identical(lexed[[1]]$kind, c("field", "separator", "field",
                                      "separator", "field")))
stopifnot(identical(lexed[[1]]$left[lexed[[1]]$kind == "field"], 1:3))
stopifnot(identical(lexed[[1]]$right[lexed[[1]]$kind == "field"], 3:1))
stopifnot(identical(lexed[[2]]$text, c("A", ",", "B")))

# Only a unique complete fixed substring is protected. Ambiguous and absent
# conditions are explicitly diagnosed and retain all possible source material.
located <- .locate_source_conditions(
  c("sample mock IFN rep1", "mock versus mock", "sample control"),
  c("mock IFN", "mock", "treated")
)
stopifnot(identical(vapply(located, `[[`, "", "status"),
                    c("unique", "ambiguous", "absent")))
stopifnot(identical(located[[1]]$protected_range$start, 8L),
          identical(located[[1]]$protected_range$end, 15L),
          isTRUE(located[[1]]$protected_range$protected),
          is.null(located[[2]]$protected_range),
          nrow(located[[2]]$matches) == 2L,
          is.null(located[[3]]$protected_range))

found <- c(
  "Found in Sample S6 F6 C", "Found in Sample S7 F7 C", "Found in Sample S8 F8 C",
  "Found in Sample S2 F2 ERU", "Found in Sample S3 F3 ERU", "Found in Sample S4 F4 ERU"
)
conditions <- c(rep("C", 3), rep("ERU", 3))
assert_same(
  build_unique_sample_names(found, conditions, "Found in Sample"),
  c("6_C", "7_C", "8_C", "2_ERU", "3_ERU", "4_ERU")
)

normalized <- sub("Found in Sample S", "Normalized Abundance F", found, fixed = TRUE)
normalized <- sub(" F[0-9]+ ", " ", normalized)
assert_same(
  build_unique_sample_names(normalized, conditions, "Normalized Abundance"),
  c("6_C", "7_C", "8_C", "2_ERU", "3_ERU", "4_ERU")
)

fixture_dir <- "tests/fixtures/regex_metadata_assistant"
quantity <- read.csv(file.path(fixture_dir, "quantity_complete_conditions.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE)
assert_same(
  build_unique_sample_names(quantity$Column, quantity$Condition, "Raw Abundance"),
  c(
    "2_mock", "3_mock", "4_mock",
    "6_mock_IFNy", "7_mock_IFNy", "8_mock_IFNy",
    "10_Capsid", "11_Capsid", "12_Capsid",
    "14_Capsid_IFNy", "15_Capsid_IFNy", "16_Capsid_IFNy",
    "18_AAV2_eGFP", "19_AAV2_eGFP", "20_AAV2_eGFP",
    "22_AAV2_eGFP_IFNy", "23_AAV2_eGFP_IFNy", "24_AAV2_eGFP_IFNy"
  )
)

# Equivalent structures deliberately replace every domain-looking literal;
# explicit identifiers may be numeric or mixed, and replicate counts may vary.
structural_fixtures <- c(
  "quantity_renamed_structure.csv", "quantity_mixed_identifiers.csv",
  "quantity_without_bracket_indices.csv", "quantity_different_replicates.csv"
)
for (fixture_name in structural_fixtures) {
  rows <- read.csv(file.path(fixture_dir, fixture_name), stringsAsFactors = FALSE,
                   check.names = FALSE)
  assert_same(build_unique_sample_names(rows$Column, rows$Condition, "Raw Abundance"),
              rows$Expected)
  forbidden <- c("SCR528", "proteome", "PG", "Quantity", "mock", "IFNy",
                 "Capsid", "AAV2", "eGFP")
  stopifnot(!any(vapply(forbidden, function(word)
    any(grepl(word, c(rows$Column, rows$Condition), fixed = TRUE)), logical(1))))
}

# Repeated conditions, configured separators, and mixed identifiers.
mixed <- c("prefix,Sample-01,C", "prefix,A_2,C", "prefix,99,C")
mixed_names <- build_unique_sample_names(mixed, rep("C", 3), "Raw Abundance", ",")
assert_same(mixed_names, c("Sample-01_C", "A_2_C", "99_C"))

# Candidate uniqueness is tested only after the complete condition is added,
# and safe numeric fields/tails outrank decorated identifiers.
ranked <- c("X002 run 41 mock", "X002 run 41 treated",
            "X003 run 42 mock", "X003 run 42 treated")
assert_same(build_unique_sample_names(ranked, rep(c("mock", "treated"), 2),
                                      "Raw Abundance"),
            c("41_mock", "41_treated", "42_mock", "42_treated"))

# When no shared representation is unique, refine just the colliding family;
# the already-unique name must not acquire the refinement suffix.
refined <- build_unique_sample_names(
  c("batch,A,C", "batch;A;C", "batch,B,D"),
  c("C", "C", "D"), "Raw Abundance", "[,;]")
stopifnot(length(unique(refined)) == 3L,
          identical(unname(refined[[3L]]), "B_D"),
          identical(attr(refined, "diagnostics")$fallback_reason[[3L]], ""))

# Identical names use the explicitly defensive fallback and remain unique.
duplicated_names <- suppressWarnings(build_unique_sample_names(
  c("same C", "same C"), c("C", "C"), "Raw Abundance"))
stopifnot(!anyDuplicated(duplicated_names))

# A shortest difference made only from separator text is not stable header
# evidence. Distinct sources use a reproducible source range, never row order;
# the row-index fallback remains reserved for byte-identical source columns.
unstable_difference <- build_unique_sample_names(
  c("same,C", "same;C"), c("C", "C"), "Raw Abundance", "[,;]")
stopifnot(!anyDuplicated(unstable_difference),
          all(grepl("same", unstable_difference, fixed = TRUE)),
          all(attr(unstable_difference, "diagnostics")$fallback_reason ==
                "source-derived differing substring"))
reordered_difference <- build_unique_sample_names(
  c("same;C", "same,C"), c("C", "C"), "Raw Abundance", "[,;]")
stopifnot(identical(sort(unname(unstable_difference)),
                    sort(unname(reordered_difference))))

# Missing conditions never generate NA or a dangling underscore.
missing_names <- build_unique_sample_names(c("run X1", "run X2"), c(NA, ""), "Raw Abundance")
assert_same(missing_names, c("X1", "X2"))

# Adversarial headers exercise row-order independence, repeated labels, leading
# zero preservation, Unicode, punctuation, regex metacharacters, and unequal
# lexical widths in one collision-free batch.
adversarial_columns <- c(
  "!! batch[alpha]+ :: 007 :: café+β",
  "!! batch[alpha]+ :: 008 :: café+β",
  "short.(009) treated?", "long.prefix.(010) treated? extra-token",
  "regex .*+?^${}()[]\\ label", "condition C C replicate 011"
)
adversarial_conditions <- c("café+β", "café+β", "treated?", "treated?", NA, "C")
adversarial <- build_unique_sample_names(
  adversarial_columns, adversarial_conditions, "Raw Abundance")
stopifnot(!anyDuplicated(adversarial), grepl("007", adversarial[[1L]], fixed = TRUE),
          grepl("008", adversarial[[2L]], fixed = TRUE),
          attr(adversarial, "diagnostics")$condition_location_status[[5L]] == "not_provided",
          attr(adversarial, "diagnostics")$condition_location_status[[6L]] == "ambiguous")

permutation <- c(6L, 2L, 5L, 1L, 4L, 3L)
reordered <- build_unique_sample_names(adversarial_columns[permutation],
  adversarial_conditions[permutation], "Raw Abundance")
restored <- character(length(permutation)); restored[permutation] <- as.character(reordered)
stopifnot(identical(as.character(adversarial), restored))

# No individual field identifies these rows, whereas the two-field prediction
# does. This also guards against implementations that assume equal conditions
# are sufficient to break candidate collisions.
paired <- build_unique_sample_names(
  c("fixed A X C", "fixed A Y C", "fixed B X C", "fixed B Y C"),
  rep("C", 4), "Raw Abundance")
stopifnot(!anyDuplicated(paired),
          all(attr(paired, "diagnostics")$candidate_tier == 4L))

# Byte-identical source headers require the explicit last-resort diagnostic.
duplicate_sources <- suppressWarnings(build_unique_sample_names(
  rep("duplicate source C", 3), rep("C", 3), "Raw Abundance"))
stopifnot(!anyDuplicated(duplicate_sources), all(grepl(
  "defensive row-index fallback", attr(duplicate_sources, "diagnostics")$fallback_reason,
  fixed = TRUE)))

# Repeated rows must use the per-call tokenization/location/prediction caches;
# counts are bounded by distinct inputs and candidate forms, not row count.
many <- suppressWarnings(build_unique_sample_names(
  rep("cached header C", 1000), rep("C", 1000), "Raw Abundance"))
cache_stats <- attr(many, "cache_stats")
stopifnot(!anyDuplicated(many), cache_stats[["tokenizations"]] == 1L,
          cache_stats[["condition_locations"]] == 1L,
          cache_stats[["candidate_predictions"]] <= 2L)

# Uniqueness is scoped by content; equal names across content types are valid.
shared <- build_unique_sample_names(c("run X1 C", "run X1 C"), c("C", "C"),
                                    c("Raw Abundance", "Found in Sample"))
assert_same(shared, c("C", "C"))

diag <- attr(mixed_names, "diagnostics")
stopifnot(is.data.frame(diag), all(c(
  "original_column", "extracted_condition", "candidate_tokens",
  "condition_match_status",
  "discarded_invariant_tokens", "selected_discriminator_tokens", "final_sample",
  "unique_within_content", "fallback_reason", "condition_start", "condition_end",
  "condition_location_status", "candidate_position", "candidate_direction",
  "source_token", "normalization", "candidate_tier", "uniqueness_scope",
  "rejected_candidate_count", "selection_reason"
) %in% names(diag)))
stopifnot(
  identical(diag$condition_start, c(18L, 12L, 11L)),
  identical(diag$condition_end, c(18L, 12L, 11L)),
  all(diag$condition_location_status == "unique"),
  all(diag$uniqueness_scope == "Raw Abundance"),
  all(diag$source_token == c("Sample-01", "A_2", "99")),
  all(nzchar(diag$selection_reason))
)

cat("unique sample-name regression checks passed\n")
