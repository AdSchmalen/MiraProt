library(testthat)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R")

fixture <- function(name) read.csv(
  file.path("tests/fixtures/regex_metadata_assistant", name),
  stringsAsFactors = FALSE, na.strings = c("NA"), check.names = FALSE
)

test_that("migrated inference matches standalone golden fixtures", {
  content <- infer_content(fixture("content.csv"))
  transformations <- setNames(content$table$Transformation, content$table$Content)
  expect_identical(unname(transformations[c("Raw Abundance", "Normalized Abundance",
    "Abundance Ratio p-Value")]), c("log2", "log10", "-log10"))

  condition_data <- fixture("condition.csv")
  condition <- infer_conditions(condition_data, "Options")
  expect_identical(condition$table$Content, "Raw Abundance")
  expect_true(all(condition$status$Status[condition$status$Content != "Raw Abundance"] == "not_applicable"))
  replay <- apply_condition_table(condition_data, condition$table, condition_data$Options)
  applicable <- replay$diagnostics$Content == "Raw Abundance"
  expect_true(all(replay$diagnostics$ExactMatch[applicable]))

  ratio_data <- fixture("ratio.csv")
  ratio <- infer_ratios(ratio_data)
  expect_identical(nrow(ratio$table), 1L)
  expect_identical(ratio$status$Status, "reliable")
  expect_identical(ratio$status$ApplicableRows, 1L)
  expect_identical(ratio$status$SuccessfulRows, 1L)
})

test_that("merged ratio families reserve cross-row structural boundaries", {
  real <- fixture("full_110_expected.csv")
  families <- c(
    "Abundance Ratio" = "Merged_Ratio_",
    "Abundance Ratio p-Value" = "Merged_Pvalue_",
    "Abundance Ratio adj. p-Value" = "Merged_Qvalue_"
  )

  check_family <- function(label, prefix) {
    rows <- real[real$Content == label & nzchar(real$Numerator) &
      nzchar(real$Denominator), , drop = FALSE]
    fit <- infer_ratios(rows)
    expect_identical(fit$status$Status, "reliable", info = label)
    expect_identical(fit$table$Method, "Regular Expressions", info = label)
    expect_identical(fit$table$NumBefore,
      regex_to_miraprot_storage(regex_atom_for_token(prefix), "ratio_boundary"),
      info = label)
    expect_identical(fit$table$NumAfter, fit$table$DenBefore, info = label)
    expect_true(is.na(fit$table$DenAfter), info = label)
    structural <- fit$diagnostics$CandidateOrigin ==
      "cross-row structural boundary"
    expect_true(any(structural), info = label)
    expect_true(all(fit$diagnostics$StructuralReservedBeforeLimit[structural]),
      info = label)
    expect_true("PruningReason" %in% names(fit$diagnostics), info = label)
    replay <- apply_ratio_table(rows, fit$table, rows$Numerator,
      rows$Denominator)
    expect_true(all(replay$diagnostics$Success), info = label)
  }

  Map(check_family, names(families), unname(families))

  combined <- real[real$Content %in% names(families) &
    nzchar(real$Numerator) & nzchar(real$Denominator), , drop = FALSE]
  fit <- infer_ratios(combined)
  expect_setequal(fit$table$Content, names(families))
  replay <- apply_ratio_table(combined, fit$table, combined$Numerator,
    combined$Denominator)
  expect_true(all(replay$diagnostics$Success))
  expect_true(all(fit$diagnostics$StructuralReservedBeforeLimit[
    fit$diagnostics$CandidateOrigin == "cross-row structural boundary"]))
})

test_that("condition inference diagnostics are warning-free when warnings are errors", {
  condition_data <- fixture("condition.csv")
  old_options <- options(warn = 2)
  on.exit(options(old_options), add = TRUE)

  expect_silent(infer_conditions(condition_data, "Options"))
})

test_that("composite condition references retain aligned structural evidence", {
  rows <- data.frame(
    Column = c("mock_IFNy_rep1", "AAV2_eGFP_IFNy_rep2"),
    Content = rep("Raw Abundance", 2L),
    Options = c("mock_IFNy", "AAV2_eGFP_IFNy"),
    stringsAsFactors = FALSE
  )

  inferred <- infer_conditions(rows, "Options")
  replay <- apply_condition_table(rows, inferred$table, rows$Options)

  expect_true(all(replay$diagnostics$ExactMatch))
  expect_true(any(inferred$diagnostics$CompositeReference %in% TRUE))
  expect_true(any(inferred$diagnostics$CandidateFamily == "reference_span"))
  selected <- inferred$diagnostics$CandidateRank == 1L
  expect_true(all(inferred$diagnostics$StructuralStability[selected] == 0L))
})

test_that("condition candidates use complete Options values during replay", {
  rows <- data.frame(
    Column = c("run_mock_IFNy_rep1", "run_mock_rep2", "run_control_rep3"),
    Content = rep("Raw Abundance", 3L),
    Options = c("mock_IFNy", "mock", "control"),
    stringsAsFactors = FALSE
  )

  inferred <- infer_conditions(rows, "Options")
  replay <- apply_condition_table(rows, inferred$table, rows$Options)

  expect_true(all(replay$diagnostics$ExactMatch))
  expect_identical(replay$metadata$Options, rows$Options)
  expect_true(any(inferred$diagnostics$CandidateFamily == "reference_span"))
  expect_true(all(inferred$diagnostics$ReplayStable[inferred$diagnostics$CandidateRank == 1L]))
  partial <- inferred$diagnostics$ExpectedCondition == "mock_IFNy" &
    inferred$diagnostics$PredictedCondition == "mock"
  expect_false(any(inferred$diagnostics$ExactMatch[partial]))
  expect_identical(names(inferred$table), CONDITION_FIELDS)
  expect_true(all(inferred$table$Method %in% CONDITION_METHODS))
})

test_that("canonical condition candidates retain identity through every replay", {
  rows <- fixture("condition_candidate_replay.csv")

  inferred <- expect_no_error(infer_conditions(rows, "Options"))
  expect_identical(names(inferred$table), CONDITION_FIELDS)
  expect_true(nrow(inferred$table) > 0L)
  expect_true(all(nzchar(inferred$table$VariantId)))
  expect_true(all(inferred$diagnostics$ReplayStable[
    !is.na(inferred$diagnostics$ReplayStable)]))

  replay <- expect_no_error(apply_condition_table(rows, inferred$table, rows$Options))
  applicable <- is_sample_bearing_content(rows$Content) & nzchar(rows$Options)
  expect_true(all(replay$diagnostics$ExactMatch[applicable]))
  expect_identical(replay$metadata$Options[applicable], rows$Options[applicable])
  expect_identical(unique(replay$diagnostics$VariantId[applicable]),
    inferred$table$VariantId)
})

test_that("condition inference exactly replays every sample-bearing row and keeps full sample names", {
  rows <- data.frame(
    Column = c(
      "raw_run_mock_IFNy_rep1", "raw_run_mock_rep2",
      "normalized_run_mock_IFNy_rep1", "normalized_run_mock_rep2",
      "found_run_mock_IFNy_rep1", "found_run_mock_rep2"
    ),
    Content = rep(c("Raw Abundance", "Normalized Abundance", "Found in Sample"),
      each = 2L),
    Options = rep(c("mock_IFNy", "mock"), 3L),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  inferred <- infer_conditions(rows, "Options")
  replay <- apply_condition_table(rows, inferred$table, rows$Options)
  sample_rows <- is_sample_bearing_content(rows$Content) & nzchar(rows$Options)

  expect_true(all(replay$diagnostics$ExactMatch[sample_rows]))
  expect_identical(replay$metadata$Options[sample_rows], rows$Options[sample_rows])
  expect_setequal(unique(replay$metadata$Options[sample_rows]), c("mock", "mock_IFNy"))
})

test_that("SCR528 structural condition rules retain complete composite Options", {
  cases <- fixture("scr528_structural_conditions.csv")
  scenarios <- split(cases, cases$Scenario)
  expected_conditions <- c(
    "mock", "mock_IFNy", "Capsid", "Capsid_IFNy", "AAV2_eGFP",
    "AAV2_eGFP_IFNy"
  )
  expect_setequal(unique(scenarios$biological_conditions$Options), expected_conditions)

  inferred <- lapply(scenarios, function(rows) infer_conditions(rows, "Options"))
  replayed <- Map(function(rows, result)
    apply_condition_table(rows, result$table, rows$Options), scenarios, inferred)

  for (scenario in names(scenarios)) {
    rows <- scenarios[[scenario]]
    result <- inferred[[scenario]]
    replay <- replayed[[scenario]]

    expect_setequal(result$table$Content,
      c("Raw Abundance", "Found in Sample", "Found in File"), info = scenario)
    expect_true(all(result$status$Status == "reliable"), info = scenario)
    expect_identical(result$status$ExactMatches, rep(18L, 3L), info = scenario)
    expect_true(all(replay$diagnostics$ExactMatch), info = scenario)
    expect_identical(replay$metadata$Options, rows$Options, info = scenario)

    # A selected rule must return the entire reference.  In particular, none
    # of the internal underscores in IFNy/eGFP composites is a terminator.
    composite <- grepl("_", rows$Options, fixed = TRUE)
    expect_identical(replay$metadata$Options[composite], rows$Options[composite],
      info = scenario)
  }

  # Changing only condition vocabulary (to unrelated names spanning one
  # through five underscore-delimited components) must not alter rule shape.
  rule_shape <- c("Content", "Method", "Before", "After", "Separators", "Pos")
  expect_identical(
    inferred$unrelated_conditions$table[, rule_shape],
    inferred$biological_conditions$table[, rule_shape]
  )
  renamed_widths <- lengths(strsplit(unique(scenarios$unrelated_conditions$Options),
    "_", fixed = TRUE))
  expect_true(all(1:5 %in% renamed_widths))

  # This tempting terminator occurs inside composite references.  Its replay
  # truncates at that internal match, so it cannot satisfy the same eligibility
  # gate (complete, exact, nonempty replay across all 18 rows) as a selected rule.
  candidate_after <- "_[[:alnum:]]+_"
  biological <- scenarios$biological_conditions
  expect_true(any(grepl(candidate_after,
    biological$Options[grepl("_", biological$Options, fixed = TRUE)], perl = TRUE)))
  candidate_rules <- transform(inferred$biological_conditions$table,
    Method = "between", Before = "_proteome_", After = candidate_after,
    Separators = "", Pos = 1L)
  candidate_replay <- apply_condition_table(biological, candidate_rules,
    biological$Options)
  candidate_eligible <- all(!candidate_replay$diagnostics$ExtractionFailure) &&
    all(candidate_replay$diagnostics$ExactMatch %in% TRUE)
  expect_false(candidate_eligible)
  expect_true(any(candidate_replay$diagnostics$PredictedCondition !=
    candidate_replay$diagnostics$ExpectedCondition))
})

test_that("18-row composite inference keeps condition extraction work linear in candidates", {
  rows <- fixture("scr528_structural_conditions.csv")
  rows <- rows[rows$Scenario == "biological_conditions",]
  original_extract <- extract_condition
  extraction_calls <- 0L
  assign("extract_condition", function(...) {
    extraction_calls <<- extraction_calls + 1L
    original_extract(...)
  }, envir = environment(infer_conditions))
  on.exit(assign("extract_condition", original_extract,
    envir = environment(infer_conditions)), add = TRUE)

  result <- infer_conditions(rows, "Options")
  per_label <- split(result$diagnostics, result$diagnostics$Content)
  generated <- sum(vapply(per_label, function(x) max(x$GeneratedCount), integer(1)))
  scored <- sum(vapply(per_label, function(x)
    length(unique(x$CandidateRank[!is.na(x$CandidateRank)])), integer(1)))

  # Each generated rule may be extracted once at admission, and each retained
  # rule is still replayed through apply_condition_table().  The final selected
  # rules receive one additional serialized completion-gate replay.  This
  # operation-count bound detects the old quadratic scan without depending on
  # machine load or an unrealistically tight elapsed-time threshold.
  expect_lte(extraction_calls, generated + scored + nrow(result$table))
  expect_true(all(result$diagnostics$ReplayStable %in% TRUE))
  expect_identical(result$status$ExactMatches, rep(18L, 3L))
})

test_that("ratio extraction distinguishes underscore-containing sample names", {
  rows <- data.frame(
    Column = c(
      "Abundance Ratio (mock_IFNy) / (mock)",
      "Abundance Ratio (mock) / (mock_IFNy)"
    ),
    Content = rep("Abundance Ratio", 2L),
    Options = rep("Ratio", 2L),
    Numerator = c("mock_IFNy", "mock"),
    Denominator = c("mock", "mock_IFNy"),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  inferred <- infer_ratios(rows)
  replay <- apply_ratio_table(rows, inferred$table, rows$Numerator, rows$Denominator)

  expect_identical(replay$metadata$Numerator, rows$Numerator)
  expect_identical(replay$metadata$Denominator, rows$Denominator)
  expect_true(all(replay$diagnostics$Success))
  expect_false(any(replay$metadata$Numerator == replay$metadata$Denominator))
})

test_that("condition token spans preserve complete references of mixed width", {
  sources <- c("prefix_mock_IFNy_suffix", "prefix_control_suffix")
  references <- c("mock_IFNy", "control")
  spans <- condition_reference_spans(sources, references)

  expect_identical(sort(unique(spans$Row)), 1:2)
  expect_identical(
    mapply(substr, sources[spans$Row], spans$Start, spans$End,
      USE.NAMES = FALSE),
    spans$Reference
  )
  expect_setequal(spans$Reference, references)
  expect_gt(spans$Width[spans$Row == 1L], spans$Width[spans$Row == 2L])

  renamed <- condition_stable_spans(
    c("prefix_unrelated_alpha_suffix", "prefix_z_suffix"),
    c("unrelated_alpha", "z")
  )
  original <- condition_stable_spans(sources, references)
  expect_identical(renamed[, c("Before", "After", "Method")],
    original[, c("Before", "After", "Method")])
})

test_that("condition reference span search is evidence bounded and contiguous", {
  references <- c("single", "one_two_three_four_five")
  sources <- paste0("before_", references, "_after")
  spans <- condition_reference_spans(sources, references)
  observed_width <- max(table(condition_token_ranges(references)$Source))

  expect_true(any(spans$Reference == references[[2L]]))
  expect_lte(max(spans$Width), as.integer(observed_width))
  expect_length(condition_reference_spans("A_intervening_B", "A_B")$Row, 0L)

  repeated <- condition_reference_spans("sample_A_B_then_A_B", "A_B")
  expect_identical(nrow(repeated), 2L)
  expect_true(all(repeated$Occurrences == 2L))
  expect_true(all(repeated$Ambiguous))
})

test_that("condition span boundaries use storage escaping and omit missing references", {
  source <- "prefix / \t(A+B) suffix"
  reference <- "A+B"
  spans <- condition_stable_spans(source, reference)

  expect_identical(nrow(spans), 1L)
  expect_identical(spans$Before, "prefix / \t(")
  expect_identical(spans$After, ") suffix")
  separators <- condition_separators(source)
  expected <- regex_to_miraprot_storage(
    regex_atom_for_token(c("/", "(", "+")),
    "condition_boundary"
  )
  expect_true(all(expected %in% separators))
  expect_true("\\s+" %in% separators)
  expect_length(condition_reference_spans(c(source, source), c("", NA_character_))$Row, 0L)
})

test_that("condition span terminators are derived at the exact reference end", {
  sources <- c(
    "prefix_alpha_1_2_1_7530.d.PG.Quantity",
    "prefix_beta_2_3_1_7531.d.PG.Quantity"
  )
  references <- c("alpha", "beta")
  spans <- condition_stable_spans(sources, references)
  pairs <- condition_stable_boundary_pairs(sources, spans)
  terminator <- pairs$After

  expect_identical(nrow(pairs), 1L)
  expect_identical(pairs$Method, "between")
  expect_match(terminator, "_1", fixed = TRUE)
  expect_false(identical("_[[:alnum:]]+_", terminator))
  locations <- mapply(function(source, pattern)
    stringr::str_locate(source, paste0("(?-i:", pattern, ")"))[["start"]],
    sources, MoreArgs = list(pattern = terminator))
  expect_identical(unname(locations), spans$End + 1L)

  embedded <- condition_stable_spans("prefix_A_1_2_1_7530.d.PG.Quantity", "A_1")
  expect_false("_[[:digit:]]+_" %in%
    attr(embedded, "boundary_candidates", exact = TRUE)$after)
})

test_that("outside-condition field candidates are structural and bounded", {
  fields <- rbind(
    c("batch7", "_", "12", ".Quantity"),
    c("batch8", "_", "13", ".Quantity")
  )
  candidates <- condition_outside_field_candidates(fields, representation_limit = 9L)

  expect_identical(candidates$Width[1:3], rep(1L, 3L))
  expect_setequal(candidates$Pattern[candidates$Width == 1L],
    c("batch[[:digit:]]{1}", "batch[[:digit:]]+", "_",
      "[[:digit:]]{2}", "[[:digit:]]+", "\\.Quantity"))
  expect_true(any(candidates$Width == 2L))
  expect_lte(nrow(candidates), 9L)
  expect_false(any(candidates$Pattern %in% c("atch", "Quan", "[[:alpha:]]+", "[[:alnum:]]+")))

  # A numeric tail is unsupported when its alphabetic prefix changes at the
  # aligned position, and condition text is never passed in as an outside field.
  changing_prefix <- matrix(c("run7", "batch8"), ncol = 1L)
  expect_identical(nrow(condition_outside_field_candidates(changing_prefix)), 0L)
  expect_false(any(grepl("alpha|beta", candidates$Pattern)))
  expect_identical(condition_outside_field_candidates(fields, 0L),
    candidates[FALSE,,drop=FALSE])
})

test_that("outside-condition identifiers use complete evidence-bounded shapes", {
  alpha <- matrix(c("cat", "dog", "owl"), ncol = 1L)
  expect_identical(condition_outside_field_candidates(alpha)$Pattern,
    c("[[:alpha:]]{1,30}", "[[:alpha:]]{3}", "(?:cat|dog|owl)"))

  carriers <- matrix(c("proteome", "secretome"), ncol = 1L)
  escaped_carriers <- paste0("(?:", paste(vapply(c("proteome", "secretome"),
    regex_escape_literal, character(1)), collapse = "|"), ")")
  expect_identical(condition_outside_field_candidates(carriers)$Pattern,
    c("[[:alpha:]]{1,30}", "[[:alpha:]]{8,9}", escaped_carriers))

  # Shapes are admitted only when each aligned value is one whole lexer
  # identifier, never when a value contains separators or token fragments.
  partial <- matrix(c("alpha-beta", "gamma-delta"), ncol = 1L)
  expect_identical(nrow(condition_outside_field_candidates(partial)), 0L)
  generated <- c(condition_outside_field_candidates(alpha)$Pattern,
    condition_outside_field_candidates(carriers)$Pattern)
  expect_false(any(generated %in% c("[[:alpha:]]+", "[[:alnum:]]+")))
  expect_true(all(c(escaped_carriers, "[[:alpha:]]{8,9}") %in% generated))

  # Policy and evidence shapes are deduplicated when their bounds coincide;
  # evidence beyond the policy maximum retains only evidence-specific forms.
  policy_bounds <- matrix(c("a", paste(rep("z", 30L), collapse = "")), ncol = 1L)
  expect_identical(condition_outside_field_candidates(policy_bounds)$Pattern,
    c("[[:alpha:]]{1,30}", paste0("(?:a|", paste(rep("z", 30L), collapse = ""), ")")))
  long <- matrix(c(paste(rep("a", 31L), collapse = ""),
    paste(rep("b", 32L), collapse = "")), ncol = 1L)
  expect_identical(condition_outside_field_candidates(long)$Pattern,
    c("[[:alpha:]]{31,32}", paste0("(?:", long[1L, 1L], "|", long[2L, 1L], ")")))
})

test_that("alphabetic carrier policy is inclusive at thirty characters", {
  carrier_30 <- c(paste(rep("a", 30L), collapse = ""),
    paste(rep("b", 7L), collapse = ""))
  candidates_30 <- condition_outside_field_candidates(matrix(carrier_30,
    ncol = 1L))$Pattern
  expect_identical(candidates_30, c("[[:alpha:]]{1,30}",
    "[[:alpha:]]{7,30}", paste0("(?:", paste(carrier_30, collapse = "|"), ")")))

  carrier_31 <- c(paste(rep("c", 31L), collapse = ""),
    paste(rep("d", 7L), collapse = ""))
  candidates_31 <- condition_outside_field_candidates(matrix(carrier_31,
    ncol = 1L))$Pattern
  expect_identical(candidates_31, c("[[:alpha:]]{7,31}",
    paste0("(?:", paste(carrier_31, collapse = "|"), ")")))
  expect_false("[[:alpha:]]{1,30}" %in% candidates_31)
  expect_false(any(candidates_31 %in% c("[[:alpha:]]+", "[[:alnum:]]+")))
})

test_that("outside-condition numeric shapes retain observed widths and fallback", {
  fixed <- condition_outside_field_candidates(matrix(c("006", "002"), ncol = 1L))
  expect_identical(fixed$Pattern, c("[[:digit:]]{3}", "[[:digit:]]+"))

  variable <- condition_outside_field_candidates(matrix(c("6", "002", "42"), ncol = 1L))
  expect_identical(variable$Pattern, c("[[:digit:]]{1,3}", "[[:digit:]]+"))

  fixed_tail <- condition_outside_field_candidates(matrix(c("X006", "X002"), ncol = 1L))
  expect_identical(fixed_tail$Pattern,
    c("X[[:digit:]]{3}", "X[[:digit:]]+"))
  variable_tail <- condition_outside_field_candidates(matrix(c("X6", "X002"), ncol = 1L))
  expect_identical(variable_tail$Pattern,
    c("X[[:digit:]]{1,3}", "X[[:digit:]]+"))
})

test_that("aligned boundary pairs preserve structure and replay exact ranges", {
  sources <- c(
    "junk_8_8_1_7000.x.PG.batch7_proteome_alpha_2_4_1_7530.d.PG.Quantity",
    "junk_7_7_1_7001.x.PG.batch8_proteome_beta_3_5_1_7531.d.PG.Quantity"
  )
  spans <- condition_stable_spans(sources,c("alpha","beta"))
  pairs <- condition_stable_boundary_pairs(sources,spans)

  expect_identical(nrow(pairs),1L)
  expect_identical(pairs$Method,"between")
  expect_match(pairs$Before,"_proteome_",fixed=TRUE)
  expect_match(pairs$After,"_1_",fixed=TRUE)
  expect_match(pairs$After,"\\.d\\.PG\\.",fixed=TRUE)
  expect_false(grepl("[[:digit:]]+",pairs$After,fixed=TRUE) &&
    !grepl("_1_",pairs$After,fixed=TRUE))
  replay <- extract_condition(sources,pairs$Method,pairs$Before,pairs$After)
  expect_identical(replay,c("alpha","beta"))

  duplicated <- c("x_proteome_alpha_tail_proteome_alpha_end",
    "y_proteome_beta_tail_proteome_beta_end")
  duplicate_spans <- condition_stable_spans(duplicated,c("alpha","beta"))
  expect_identical(nrow(condition_stable_boundary_pairs(duplicated,duplicate_spans)),0L)
})

test_that("aligned boundary pairs try evidence shapes after unsafe alpha policy", {
  references <- c("alpha", "beta")
  sources <- c("ox!dog!alpha#tail", "an!cat!beta#tail")
  spans <- condition_reference_spans(sources, references)
  pairs <- condition_stable_boundary_pairs(sources, spans)

  expect_identical(nrow(pairs), 1L)
  expect_identical(pairs$Before, "[[:alpha:]]{3}!")
  expect_false(grepl("{1,30}", pairs$Before, fixed = TRUE))
  expect_identical(extract_condition(sources, pairs$Method, pairs$Before,
    pairs$After), references)

  sources <- c("pig!dog!alpha#tail", "hen!cat!beta#tail")
  pairs <- condition_stable_boundary_pairs(sources,
    condition_reference_spans(sources, references))
  expect_identical(pairs$Before, "(?:dog|cat)!")
  expect_identical(extract_condition(sources, pairs$Method, pairs$Before,
    pairs$After), references)
})

test_that("aligned separator runs generalize only observed whitespace slots", {
  check_variant <- function(whitespace, expected_atom) {
    references <- c("mock_IFNy", "mock")
    sources <- c(
      paste0("SCR528_proteome_", references[[1L]], "_tail"),
      paste0("SCR529_proteome", whitespace, "_", references[[2L]], "_tail")
    )
    spans <- condition_reference_spans(sources, references)
    pairs <- condition_stable_boundary_pairs(sources, spans)

    expect_identical(nrow(pairs), 1L)
    expect_match(pairs$Before, expected_atom, fixed = TRUE)
    locations <- stringr::str_locate_all(sources,
      paste0("(?-i:", pairs$Before, ")"))
    expect_true(all(lengths(locations) == 2L))
    expect_identical(vapply(locations,function(x)x[[1L,"end"]]+1L,integer(1)),
      spans$Start)
    expect_identical(extract_condition(sources,pairs$Method,pairs$Before,pairs$After),
      references)
    pairs
  }

  space <- check_variant(" ", "\\s*_")
  check_variant("\t", "\\s*_")
  check_variant("\u00A0", "\\s*_")
  expect_identical(attr(space,"boundary_evidence")$Before,
    c('"proteome"','"_"','"proteome"','" _"'))
  expect_identical(condition_boundary_evidence_render(c("_", " _", "\t_", "\u00A0_")),
    c('"_"','" _"','"\\t_"','"\\u00A0_"'))
})

test_that("after boundaries classify NBSP with persisted regex semantics", {
  references <- c("mock", "AAV2_eGFP_IFNy")
  sources <- c(
    "SCR529_X002_secretome_mock_1\u00A0_2_1_7559.d.PG.Quantity",
    "SCR529_X022_secretome_AAV2_eGFP_IFNy_1_22_1_7579.d.PG.Quantity"
  )
  spans <- condition_reference_spans(sources, references)
  pairs <- condition_stable_boundary_pairs(sources, spans)

  expect_identical(nrow(pairs), 1L)
  expect_match(pairs$After, "\\s*_", fixed = TRUE)
  expect_false("separator_incompatibility" %in%
    vapply(attr(pairs, "boundary_diagnostics"), `[[`, character(1L), "Kind"))
  expect_identical(extract_condition(sources, pairs$Method, pairs$Before,
    pairs$After), references)
})

test_that("aligned separator runs reject incompatible punctuation skeletons", {
  incompatible <- list(c("_", "-_"), c("__", "_"), c("_-", "-_"))
  for(runs in incompatible) {
    references <- c("mock_IFNy", "mock")
    sources <- paste0("shared", runs, references)
    spans <- condition_reference_spans(sources, references)
    expect_identical(nrow(condition_stable_boundary_pairs(sources, spans)), 0L,
      info = paste(runs, collapse = " versus "))
  }

  references <- c("mock_IFNy", "mock")
  sources <- paste0("shared", c("\t-_", "\u00A0_-"), references)
  pairs <- condition_stable_boundary_pairs(sources,
    condition_reference_spans(sources, references))
  records <- attr(pairs, "boundary_diagnostics")
  expect_identical(nrow(pairs), 0L)
  expect_identical(length(records), 1L)
  expect_identical(records[[1L]]$Kind, "separator_incompatibility")
  expect_identical(records[[1L]]$Side, "before")
  expect_identical(records[[1L]]$AlignedFieldDepth, 1L)
  expect_identical(records[[1L]]$TokenType, "separator")
  expect_identical(records[[1L]]$DetectedShape, "punctuation_skeleton")
  expect_identical(records[[1L]]$ObservedRuns, c("\\t-_", "\\u00A0_-"))
  expect_identical(records[[1L]]$PunctuationSkeletons, c("-_", "_-"))
})

test_that("unsupported outside identifiers expose structured diagnostics", {
  references <- c("mock_IFNy", "mock")
  sources <- paste0(c("X006", "Y002"), references)
  pairs <- condition_stable_boundary_pairs(sources,
    condition_reference_spans(sources, references))
  records <- attr(pairs, "boundary_diagnostics")

  expect_identical(nrow(pairs), 0L)
  expect_identical(length(records), 1L)
  expect_identical(records[[1L]]$Kind,
    "unsupported_outside_field_variation")
  expect_identical(records[[1L]]$Side, "before")
  expect_identical(records[[1L]]$AlignedFieldDepth, 1L)
  expect_identical(records[[1L]]$TokenType, "identifier")
  expect_identical(records[[1L]]$DetectedShape, "alphanumeric")
  expect_identical(records[[1L]]$Examples, c("X006", "Y002"))
})

test_that("SCR528 and SCR529 separator reproduction has one exact boundary", {
  references <- c("mock_IFNy", "mock")
  sources <- c(
    "[6] SCR528_X002_proteome_mock_IFNy_1_S02_1_5221.d.PG.Quantity",
    "[1] SCR529_X001_proteome _mock_1_S01_1_5211.d.PG.Quantity"
  )
  spans <- condition_reference_spans(sources, references)
  pairs <- condition_stable_boundary_pairs(sources, spans)

  expect_identical(nrow(pairs), 1L)
  expect_match(pairs$Before, "proteome\\s*_", fixed = TRUE)
  locations <- stringr::str_locate_all(sources,paste0("(?-i:",pairs$Before,")"))
  expect_true(all(lengths(locations) == 2L))
  expect_identical(vapply(locations,function(x)x[[1L,"end"]]+1L,integer(1)),spans$Start)
  expect_identical(extract_condition(sources,pairs$Method,pairs$Before,pairs$After),
    references)
})

test_that("reference-span ambiguity follows ordered extractor pairs", {
  safe<-condition_between_pair_diagnostics("pre_alpha_end_pre", "pre_", "_end", "alpha")
  expect_true(safe$BoundaryOccurrenceAmbiguity)
  expect_false(safe$PairAmbiguity)
  expect_false(safe$LocatedSpanAmbiguity)

  earlier<-condition_between_pair_diagnostics("pre_junk_pre_alpha_end", "pre_", "_end", "alpha")
  expect_true(earlier$BoundaryOccurrenceAmbiguity)
  expect_false(earlier$PairAmbiguity)
  expect_true(earlier$LocatedSpanAmbiguity)
})

test_that("reference span boundaries expand complete tokens beyond legacy context width", {
  long_token <- paste(rep("/", 20L), collapse = "")
  sources <- paste0(long_token, c("alpha", "beta"), "_tail")
  spans <- condition_stable_spans(sources, c("alpha", "beta"))
  pairs <- condition_stable_boundary_pairs(sources, spans)

  expect_identical(nrow(pairs), 1L)
  expect_identical(pairs$Before, regex_escape_literal(long_token))
  expect_gt(nchar(pairs$Before), CONDITION_CONTEXT_SEARCH_WIDTH)
  expect_identical(extract_condition(sources, pairs$Method, pairs$Before, pairs$After),
    c("alpha", "beta"))

  old_width <- CONDITION_CONTEXT_SEARCH_WIDTH
  on.exit(assign("CONDITION_CONTEXT_SEARCH_WIDTH", old_width,
    envir = environment(condition_contexts)), add = TRUE)
  assign("CONDITION_CONTEXT_SEARCH_WIDTH", 1L,
    envir = environment(condition_contexts))
  expect_identical(condition_stable_boundary_pairs(sources, spans), pairs)
  expect_identical(nrow(condition_stable_boundary_pairs(sources, spans,
    representation_limit = 0L)), 0L)
})

test_that("primary entrypoint has a pure diagnostic contract", {
  messages <- list()
  result <- auto_regex_infer_rules(fixture("content.csv"), condition_target = "",
    debug_log = function(message, level) messages[[length(messages) + 1L]] <<- c(message, level))
  expect_named(result, c("rules", "statuses", "diagnostics", "warnings", "errors", "timings"))
  expect_named(result$rules, c("table", "condition", "ratio"))
  expect_identical(names(result$rules$table), CONTENT_FIELDS)
  expect_type(result$timings, "double")
  expect_true(length(messages) > 0L)
})

test_that("Identifier vocabulary fallback is bounded, last-resort, and score gated", {
  original_fragments <- candidate_fragments
  on.exit(assign("candidate_fragments", original_fragments, envir=.GlobalEnv), add=TRUE)
  assign("candidate_fragments", function(...) data.frame(Pattern=character(), Family=character()),
    envir=.GlobalEnv)

  accepted <- data.frame(
    Column=c("Protein accession", "Gene ID", "confidence", "modified",
      "identification score", "sample name", "gene-related description"),
    Content=c("Identifier", "Identifier", rep("Metadata", 5L)),
    stringsAsFactors=FALSE, check.names=FALSE)
  messages <- character()
  inferred <- infer_content(accepted, logger=function(level, component, step, message, ...)
    messages <<- c(messages, paste(level, component, step, message)))
  identifier <- inferred$table[inferred$table$Content=="Identifier",,drop=FALSE]
  expect_identical(nrow(identifier), 1L)
  expect_match(identifier$Include, "\\(\\?i:")
  expect_false(safe_grepl(identifier$Include, "identification score"))
  expect_true(any(grepl("fallback generated", messages, fixed=TRUE)))
  expect_identical(sum(grepl("fallback accepted", messages, fixed=TRUE)), 1L)

  # Exact label matching prevents fallback use for near names, and a single
  # positive row or any false positive remains below the ordinary gate.
  near_label <- accepted; near_label$Content[near_label$Content=="Identifier"] <- "identifier"
  near_messages <- character()
  near <- infer_content(near_label, logger=function(level, component, step, message, ...)
    near_messages <<- c(near_messages, message))
  expect_false(any(grepl("Identifier fallback", near_messages, fixed=TRUE)))
  expect_false("identifier" %in% near$table$Content)

  ambiguous <- data.frame(Column=c("Gene ID", "subject id"),
    Content=c("Identifier", "Metadata"), stringsAsFactors=FALSE, check.names=FALSE)
  rejected <- infer_content(ambiguous)
  expect_false("Identifier" %in% rejected$table$Content)
  expect_true("Identifier" %in% rejected$unresolved_reasons$Content)
  expect_true(any(grepl("Identifier:", rejected$warnings, fixed=TRUE)))
})

test_that("accepted integrated Identifier fallback is not reported as a warning", {
  original_fragments <- candidate_fragments
  on.exit(assign("candidate_fragments", original_fragments, envir=.GlobalEnv), add=TRUE)
  assign("candidate_fragments",
    function(...) data.frame(Pattern=character(), Family=character()),
    envir=.GlobalEnv)

  metadata <- data.frame(
    Column=c("Protein accession", "Other metadata"),
    Content=c("Identifier", "Metadata"),
    stringsAsFactors=FALSE, check.names=FALSE
  )
  result <- auto_regex_infer_rules(metadata, condition_target="")

  expect_true("Identifier" %in% result$rules$table$Content)
  expect_true(nrow(result$diagnostics$identifier_fallback) > 0L)
  expect_false(any(startsWith(result$warnings, "Identifier: ")))
})

test_that("accepted technical Description fallback is information, not warning", {
  metadata <- data.frame(
    Column = c("Description", "Raw A"),
    Content = c("Description", "Raw Abundance"),
    Options = c("", "A"), stringsAsFactors = FALSE, check.names = FALSE
  )
  result <- auto_regex_infer_rules(metadata, condition_target = "Options")
  expect_true("Description" %in% result$rules$table$Content)
  expect_true(any(result$diagnostics$technical_singleton_fallback$Applied))
  expect_false(any(startsWith(result$warnings, "Description: ")))
})

test_that("semantic spans require unique, bounded, rule-confirmed source evidence", {
  rows <- fixture("semantic_spans.csv")
  ratios <- data.frame(Content="Abundance Ratio",Method="Position in String",
    Separators="\\s+|\\(|\\)|/",Invert=FALSE,NumBefore=NA_character_,
    NumAfter=NA_character_,DenBefore=NA_character_,DenAfter=NA_character_,
    NumPos=1L,DenPos=2L,stringsAsFactors=FALSE,check.names=FALSE)
  spans <- auto_regex_semantic_spans(rows,empty_condition(),data.frame(),ratios,data.frame())

  first <- spans[spans$Row==1L,,drop=FALSE]
  expect_identical(first$Reference,c("ERU","C"))
  expect_true(all(first$SafeToGeneralize))
  expect_identical(first$ReplacementAtom,c("[^()]+","[^()]+"))
  expect_identical(first$Start,c(2L,10L))
  expect_true(all(grepl("exact ratio tokenizer position",first$Diagnostic,fixed=TRUE)))

  # A second C must make fixed source mapping ambiguous rather than allowing the
  # positional rule to generalise whichever occurrence happens to be found.
  repeated <- spans[spans$Row==2L & spans$Semantic=="denominator",,drop=FALSE]
  expect_false(repeated$SafeToGeneralize)
  expect_match(repeated$Diagnostic,"more than once",fixed=TRUE)

  # Unbounded references, including mixed shapes and Unicode before the span,
  # retain exact character offsets but are not declared safe.
  unbounded <- spans[spans$Row==3L & spans$Semantic=="numerator",,drop=FALSE]
  expect_identical(unbounded$Start,2L)
  expect_false(unbounded$SafeToGeneralize)
})

test_that("semantic span atoms cover bracket types and varied condition values", {
  rows <- data.frame(Column=c("raw[alpha 12-A_+]tail","raw{long-value_2+ X}tail"),
    Content=rep("Raw Abundance",2L),Options=c("alpha 12-A_+","long-value_2+ X"),
    Numerator="",Denominator="",stringsAsFactors=FALSE,check.names=FALSE)
  rule <- data.frame(Content="Raw Abundance",Method="between",Before="raw\\[|raw\\{",
    After="\\]tail|\\}tail",Separators="",Pos=1L,stringsAsFactors=FALSE,check.names=FALSE)
  spans <- auto_regex_semantic_spans(rows,rule,data.frame(),empty_ratio(),data.frame())
  expect_true(all(spans$SafeToGeneralize))
  expect_identical(spans$ReplacementAtom,c("[^\\[\\]]+","[^{}]+"))
  expect_true(all(spans$ExtractionConfirmed))
})

test_that("semantic content refinement preserves evidence and complete-table conflicts", {
  rows <- data.frame(
    Column=c("Ratio (A)/(B)","Ratio (C)/(D)","P-Value Ratio (X)/(Y)","Other"),
    Content=c("Abundance Ratio","Abundance Ratio","Abundance Ratio p-Value","Other"),
    stringsAsFactors=FALSE,check.names=FALSE)
  rules <- data.frame(Content=c("Abundance Ratio","Abundance Ratio p-Value","Other"),
    Include=c("^Ratio \\(A\\)/\\(B\\)$","^P-Value Ratio","^Other$"),Exclude=c("","",""),
    Transformation=c("log2","-log10",NA_character_),stringsAsFactors=FALSE,check.names=FALSE)
  make_spans <- function(row, values) do.call(rbind,lapply(seq_along(values),function(i) {
    start <- regexpr(values[[i]],rows$Column[[row]],fixed=TRUE)[[1L]]
    data.frame(Row=row,Content=rows$Content[[row]],Semantic=if(i==1L)"numerator" else "denominator",
      Reference=values[[i]],Start=start,End=start+nchar(values[[i]])-1L,
      ReplacementAtom="[^()]+",SafeToGeneralize=TRUE,stringsAsFactors=FALSE)
  }))
  spans <- rbind(make_spans(1L,c("A","B")),make_spans(2L,c("C","D")))
  refined <- refine_content_with_semantic_spans(rows,rules,spans)

  ratio <- refined$lineage[refined$lineage$Content=="Abundance Ratio",,drop=FALSE]
  expect_true(ratio$Accepted)
  expect_identical(refined$table$Transformation,rules$Transformation)
  expect_identical(refined$table[2:3,CONTENT_FIELDS],rules[2:3,CONTENT_FIELDS])
  metric <- score_pattern(ratio$FinalInclude,rows$Column,rows$Content=="Abundance Ratio",ratio$FinalExclude)
  expect_identical(as.integer(metric[1L,c("TP","FP","FN")]),c(2L,0L,0L))
  expect_false(any(refined$application$rows$Conflict))
})

test_that("content semantic refinement does not absorb condition tokens", {
  rows <- data.frame(
    Column = c("Raw signal [mock]", "Raw signal [mock_IFNy]", "Other metadata"),
    Content = c("Raw Abundance", "Raw Abundance", "Metadata"),
    Options = c("mock", "mock_IFNy", ""),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  rules <- data.frame(
    Content = c("Raw Abundance", "Metadata"),
    Include = c("^Raw signal \\[mock\\]$", "^Other metadata$"),
    Exclude = c("", ""), Transformation = c("log2", NA_character_),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  spans <- do.call(rbind, lapply(1:2, function(row) {
    reference <- rows$Options[[row]]
    start <- regexpr(reference, rows$Column[[row]], fixed = TRUE)[[1L]]
    data.frame(Row = row, Content = "Raw Abundance", Semantic = "condition",
      Reference = reference, Start = start, End = start + nchar(reference) - 1L,
      ReplacementAtom = "[^\\[\\]]+", SafeToGeneralize = TRUE,
      stringsAsFactors = FALSE, check.names = FALSE)
  }))

  refined <- refine_content_with_semantic_spans(rows, rules, spans)
  raw <- refined$lineage[refined$lineage$Content == "Raw Abundance", , drop = FALSE]

  expect_true(raw$Accepted)
  expect_false(grepl("mock", raw$FinalInclude, fixed = TRUE))
  expect_true(all(safe_grepl(raw$FinalInclude, rows$Column[1:2])))
  expect_false(safe_grepl(raw$FinalInclude, rows$Column[[3L]]))
})

test_that("semantic refinement rejects a prospective false positive", {
  rows <- data.frame(Column=c("Measure (A)","Measure (B)","Measure (noise)"),
    Content=c("Target","Target","Negative"),stringsAsFactors=FALSE,check.names=FALSE)
  rules <- data.frame(Content=c("Target","Negative"),Include=c("^Measure \\(A\\)$","noise"),
    Exclude=c("",""),Transformation=c(NA_character_,NA_character_),stringsAsFactors=FALSE,check.names=FALSE)
  spans <- do.call(rbind,lapply(1:2,function(row) data.frame(Row=row,Content="Target",Semantic="condition",
    Reference=substr(rows$Column[[row]],10L,10L),Start=10L,End=10L,ReplacementAtom="[^()]+",
    SafeToGeneralize=TRUE,stringsAsFactors=FALSE)))
  refined <- refine_content_with_semantic_spans(rows,rules,spans)
  target <- refined$lineage[refined$lineage$Content=="Target",,drop=FALSE]
  expect_false(target$Accepted)
  expect_identical(target$FinalInclude,target$OriginalInclude)
  expect_match(target$RejectionReason,"false_positives")
})

test_that("ratio prerequisite injection is equivalent to standalone fallback", {
  metadata <- Reduce(function(x, y) merge(x, y, all = TRUE),
    lapply(c("content.csv", "condition.csv", "ratio.csv"), fixture))
  content <- infer_content(metadata)
  condition <- infer_conditions(metadata, "Options")
  fallback <- infer_ratios(metadata)
  supplied <- infer_ratios(metadata, content_rules = content$table,
    condition_rules = condition$table)

  expect_identical(supplied$table, fallback$table)
  expect_identical(supplied$status, fallback$status)
  expect_identical(supplied$diagnostics, fallback$diagnostics)
  expect_identical(supplied$warnings, fallback$warnings)
  expect_named(supplied$timings, c("prerequisite_application",
    "candidate_construction", "candidate_scoring", "completion_gate_replay"))
  malformed <- content$table[, setdiff(names(content$table), "Exclude"), drop = FALSE]
  expect_error(infer_ratios(metadata, content_rules = malformed,
    condition_rules = condition$table), "canonical data frame")
})

test_that("representative integrated run executes prerequisite phases once", {
  rows <- data.frame(
    Column = sprintf("Raw Abundance sample %02d", seq_len(46L)),
    Content = rep("Raw Abundance", 46L),
    Options = sprintf("sample %02d", seq_len(46L)),
    Numerator = "", Denominator = "", stringsAsFactors = FALSE,
    check.names = FALSE)
  messages <- character()
  result <- auto_regex_infer_rules(rows, condition_target = "Options",
    debug_log = function(message, level) messages <<- c(messages, message))

  expect_length(result$errors, 0L)
  expect_identical(sum(grepl("Inferring content rules from", messages, fixed = TRUE)), 1L)
  expect_identical(sum(grepl("Inferring condition rules from", messages, fixed = TRUE)), 1L)
})

validation_payload <- function(condition) coerce_contract(list(
  table = data.frame(Content = "Row Index", Include = "Row Index", Exclude = "",
    Transformation = NA_character_, stringsAsFactors = FALSE, check.names = FALSE),
  condition = condition
))

condition_rule <- function(method, before = "", after = "") data.frame(
  Content = "Raw Abundance", Method = method, Before = before, After = after,
  Separators = "", Pos = 1L, stringsAsFactors = FALSE, check.names = FALSE)

canonical_export_contract <- function() list(
  table = data.frame(
    RuleId = c("row-index-r1", "raw-r1", "ratio-r1"),
    Content = c("Row Index", "Raw Abundance", "Abundance Ratio"),
    VariantId = c("row-index-v1", "raw-v1", "ratio-v1"),
    Priority = 0:2,
    Include = c("Row Index", "Raw", "Ratio"), Exclude = c("", "", ""),
    Transformation = c(NA_character_, "None", "None"),
    stringsAsFactors = FALSE, check.names = FALSE),
  condition = data.frame(
    RuleId = "condition-r1", Content = "Raw Abundance", VariantId = "raw-v1",
    Method = "end", Before = "_", After = "", Separators = "", Pos = 1L,
    stringsAsFactors = FALSE, check.names = FALSE),
  ratio = data.frame(
    RuleId = "ratio-rule-r1", Content = "Abundance Ratio", VariantId = "ratio-v1",
    Method = "Regular Expressions", Separators = NA_character_, Invert = FALSE,
    NumBefore = "", NumAfter = "/", DenBefore = "", DenAfter = "",
    NumPos = NA_integer_, DenPos = NA_integer_, stringsAsFactors = FALSE,
    check.names = FALSE)
)

test_that("canonical empty and populated contracts coerce and validate", {
  empty <- coerce_contract(list(
    table = empty_content(), condition = empty_condition(), ratio = empty_ratio()))
  populated <- coerce_contract(canonical_export_contract())

  expect_length(validate_export(empty), 0L)
  expect_length(validate_export(populated), 0L)
  expect_identical(lapply(empty, names), list(
    table = CONTENT_FIELDS, condition = CONDITION_FIELDS, ratio = RATIO_FIELDS))
})

component_cardinality_contract <- function(component, rows) {
  contract <- canonical_export_contract()
  contract$condition <- empty_condition()
  contract$ratio <- empty_ratio()
  if (component == "table") {
    contract$table <- if (rows == 0L) empty_content() else
      contract$table[seq_len(rows), , drop = FALSE]
  } else {
    prototype <- contract[[component]]
    contract[[component]] <- if (rows == 0L) prototype[FALSE, , drop = FALSE] else {
      result <- prototype[rep(1L, rows), , drop = FALSE]
      result$RuleId <- sprintf("%s-cardinality-r%d", component, seq_len(rows))
      row.names(result) <- NULL
      result
    }
  }
  contract
}

test_that("empty, one-row, and multi-row components share the coercion and validation contract", {
  for (component in c("table", "condition", "ratio")) {
    for (rows in 0:2) {
      coerced <- coerce_contract(component_cardinality_contract(component, rows))
      expected_classes <- canonical_rule_classes()

      expect_identical(nrow(coerced[[component]]), rows,
        info = sprintf("%s with %d rows", component, rows))
      expect_identical(lapply(coerced, function(x)
        vapply(x, function(column) class(column)[1L], character(1))),
        expected_classes, info = sprintf("%s with %d rows", component, rows))
      expect_length(validate_export(coerced), 0L,
        info = sprintf("%s with %d rows", component, rows))
    }
  }
})

header_family_rules <- function() {
  rules <- canonical_export_contract()
  raw <- rules$table[rules$table$Content == "Raw Abundance", , drop = FALSE]
  raw$RuleId <- "content-raw-sibling"
  raw$VariantId <- "raw-sibling"
  raw$Priority <- max(rules$table$Priority) + 1L
  raw$Include <- "^raw_b$"
  rules$table$Include[rules$table$Content == "Raw Abundance"] <- "^raw_a$"
  rules$table <- rbind(rules$table, raw)
  rules
}

test_that("sibling header-family variants accept the same transformation", {
  rules <- header_family_rules()
  expect_length(validate_export(rules), 0L)
})

test_that("contradictory sibling transformations replay by winning variant", {
  rules <- header_family_rules()
  # A deliberately contradictory sibling is valid: Content is only a display
  # label, while VariantId selects the effective transformation.
  rules$table$Transformation[rules$table$VariantId == "raw-sibling"] <- "log10"
  expect_length(validate_export(rules), 0L)
  replay <- apply_content_table(data.frame(
    Column = c("raw_a", "raw_b"), Content = rep("Raw Abundance", 2L),
    Transformation = c("log2", "log10"), stringsAsFactors = FALSE), rules$table)
  expect_identical(replay$metadata$Transformation, c("log2", "log10"))
  expect_identical(attr(replay$metadata, "variant_id"),
    c(rules$table$VariantId[rules$table$Include == "^raw_a$"], "raw-sibling"))
})

test_that("export class validation reports named expected and actual schemas", {
  malformed <- canonical_export_contract()
  malformed$table$Priority <- as.character(malformed$table$Priority)
  malformed$condition$Pos <- as.character(malformed$condition$Pos)
  malformed$ratio$Invert <- as.character(malformed$ratio$Invert)

  errors <- validate_export(malformed)

  expect_true(any(grepl("table column classes", errors, fixed = TRUE) &
    grepl("Priority=integer", errors, fixed = TRUE) &
    grepl("Priority=character", errors, fixed = TRUE)))
  expect_true(any(grepl("condition column classes", errors, fixed = TRUE) &
    grepl("Pos=integer", errors, fixed = TRUE) &
    grepl("Pos=character", errors, fixed = TRUE)))
  expect_true(any(grepl("ratio column classes", errors, fixed = TRUE) &
    grepl("Invert=logical", errors, fixed = TRUE) &
    grepl("Invert=character", errors, fixed = TRUE)))
  expect_true(any(grepl("table column classes", errors, fixed = TRUE) &
    grepl("field=Priority", errors, fixed = TRUE)))
  expect_true(any(grepl("condition column classes", errors, fixed = TRUE) &
    grepl("field=Pos", errors, fixed = TRUE)))
  expect_true(any(grepl("ratio column classes", errors, fixed = TRUE) &
    grepl("field=Invert", errors, fixed = TRUE)))
})

test_that("regex boundary presence preserves literal whitespace", {
  valid <- list(
    condition_rule("end", before = " "),
    condition_rule("end", before = "\\s+"),
    condition_rule("end", before = ", "),
    condition_rule("start", after = " "),
    condition_rule("between", before = " ", after = "suffix"),
    condition_rule("between", before = "prefix", after = " "),
    condition_rule("between", before = " ", after = " ")
  )
  for (rule in valid) expect_length(validate_export(validation_payload(rule)), 0L)

  for (boundary in list("", NA_character_)) {
    errors <- validate_export(validation_payload(condition_rule("end", before = boundary)))
    expect_true(any(grepl("requires Before", errors, fixed = TRUE)))
  }
})

test_that("validation identifies each invalid rule cell and aggregates the run failure", {
  rules <- validation_payload(empty_condition())
  rules$table <- rbind(
    rules$table,
    data.frame(Content = c("Label A", "Label B"), Include = c("(", "["),
      Exclude = c("", ""), Transformation = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE, check.names = FALSE))
  logs <- character()
  errors <- validate_export(rules, logger=function(level, component, step, message, ...)
    logs <<- c(logs, message))

  expect_true(any(grepl("Content=\"Label A\"", errors, fixed=TRUE) &
    grepl("field=Include", errors, fixed=TRUE)))
  expect_true(any(grepl("Content=\"Label B\"", errors, fixed=TRUE) &
    grepl("field=Include", errors, fixed=TRUE)))
  expect_identical(sum(grepl("Rule validation failed:", errors, fixed=TRUE)), 1L)
  expect_true(any(grepl("Label A", logs, fixed=TRUE)))
  expect_true(any(grepl("Label B", logs, fixed=TRUE)))
})

test_that("diagnostic values distinguish missing, empty, and literal whitespace", {
  expect_identical(auto_regex_diagnostic_value(""), "<empty>")
  expect_identical(auto_regex_diagnostic_value(NA_character_), "<NA>")
  expect_match(auto_regex_diagnostic_value(" "), '^" "; escaped="\\\\x20"$', perl=TRUE)
  expect_match(auto_regex_diagnostic_value("\\s+"), "\\\\\\\\s\\+", perl=TRUE)
})

test_that("condition boundary representation ranking is exact and deterministic", {
  cases <- fixture("condition_boundaries.csv")
  logs <- character()
  inferred <- lapply(split(cases, cases$Scenario), function(rows) {
    result <- infer_conditions(rows, "Options", logger = function(level, stage, event, message, ...) {
      if (identical(event, "score")) logs <<- c(logs, message)
    })
    replay <- apply_condition_table(rows, result$table, rows$Options)
    expect_true(all(replay$diagnostics$ExactMatch))
    result
  })

  for (scenario in c("one_space", "repeated_spaces", "tabs", "comma_whitespace")) {
    rule <- inferred[[scenario]]$table
    expect_true(any(grepl("\\s+", unlist(rule[c("Before", "After", "Separators")]), fixed = TRUE)))
  }
  # Spaces within the class value are not rewritten: only a transferred
  # boundary may be generalized.
  expect_identical(apply_condition_table(cases[cases$Scenario == "class_phrase",],
    inferred$class_phrase$table)$metadata$Options,
    cases$Options[cases$Scenario == "class_phrase"])

  rejected <- inferred$false_generalization$diagnostics
  generalized <- grepl("\\s+", rejected$Before, fixed = TRUE) |
    grepl("\\s+", rejected$After, fixed = TRUE) |
    grepl("\\s+", rejected$Separators, fixed = TRUE)
  expect_true(any(generalized & (rejected$Ambiguous | !rejected$ExactMatch)))
  expect_false(grepl("\\s+", paste(unlist(inferred$false_generalization$table), collapse = ""), fixed = TRUE))
  expect_true(any(grepl("representation rank", logs, fixed = TRUE)))
  expect_true(any(grepl("Before=", logs, fixed = TRUE) & grepl("Separators=", logs, fixed = TRUE)))
})

test_that("reference span search is evidence-width bounded and reports exhaustion", {
  long_reference <- paste(rep("supported", 20L), collapse = "-")
  spans <- condition_reference_spans(paste0("prefix/", long_reference, "/suffix"),
    long_reference, representation_limit = 100L)
  expect_equal(spans$Width, 39L)
  expect_equal(attr(spans, "span_diagnostics")$largest_evidence_span_width, 39L)

  repeated <- condition_reference_spans(rep("prefix/value/suffix", 3L),
    rep("value", 3L), representation_limit = 100L)
  expect_equal(nrow(repeated), 3L)
  expect_equal(attr(repeated, "span_diagnostics")$deduplicated, 2L)

  rows <- data.frame(Column = paste(rep("x", 30L), collapse = "-"),
    Content = "Raw Abundance", Options = "missing", stringsAsFactors = FALSE)
  old_limit <- CONDITION_SPAN_REPRESENTATION_LIMIT
  on.exit(assign("CONDITION_SPAN_REPRESENTATION_LIMIT", old_limit,
    envir = environment(condition_reference_spans)), add = TRUE)
  assign("CONDITION_SPAN_REPRESENTATION_LIMIT", 0L,
    envir = environment(condition_reference_spans))
  result <- infer_conditions(rows, "Options")
  expect_equal(nrow(result$table), 0L)
  expect_match(result$status$UnresolvedReason, "representation limit exhausted", fixed = TRUE)
})

test_that("content redundancy does not override an exact structural condition boundary", {
  rows <- fixture("condition_boundaries.csv")
  rows <- rows[rows$Scenario == "repeated_spaces",]
  result <- auto_regex_infer_rules(rows, condition_target = "Options", redundancy = 3L)
  boundary <- unlist(result$rules$condition[c("Before", "After", "Separators")])
  expect_true(any(grepl("\\s+", boundary, fixed = TRUE)))
})

test_that("post-legacy diagnostics gate grouping with row-level failure evidence", {
  rows <- data.frame(Column = c("alpha-A", "beta(1)", "other"),
    Content = c("Raw Abundance", "Raw Abundance", "Identifier"),
    Options = c("", "", ""), stringsAsFactors = FALSE)
  application <- apply_content_table(rows, empty_content())
  content <- list(table=empty_content(), conflicts=list(application=application$rows),
    unresolved_reasons=data.frame(Content="Raw Abundance", Code="no_reliable_candidate",
      Reason="legacy rule failed", stringsAsFactors=FALSE))
  condition <- list(table=empty_condition(), status=data.frame(), diagnostics=data.frame())
  ratio <- list(table=empty_ratio(), status=data.frame(), diagnostics=data.frame())

  diagnostic <- auto_regex_failure_diagnostic(rows,content,condition,ratio,"Options")
  raw <- diagnostic$by_content[diagnostic$by_content$Content=="Raw Abundance",]
  expect_true(raw$ContentRuleFailure)
  expect_equal(raw$StructuralFamilyCount,2L)
  expect_true(raw$SafeSubsetEvidence)
  expect_true(raw$GroupedFallbackEligible)
  expect_setequal(diagnostic$by_row$Row[diagnostic$by_row$ContentFailed],1:3)

  rows$Column[[3L]] <- "alpha-A"
  content$conflicts$application <- apply_content_table(rows,empty_content())$rows
  diagnostic <- auto_regex_failure_diagnostic(rows,content,condition,ratio,"Options")
  raw <- diagnostic$by_content[diagnostic$by_content$Content=="Raw Abundance",]
  expect_true(raw$ConflictingLabelsForIdenticalSource)
  expect_false(raw$GroupedFallbackEligible)
})

test_that("ratio reference obligations are evaluated per structural family", {
  rows <- data.frame(Column=c("alpha-A","alpha-B","beta(1)","beta(2)","other"),
    Content=c(rep("Raw Abundance",4L),"Identifier"), Options="",
    Numerator=c("A","B","","",""), Denominator=c("ref","ref","","",""),
    stringsAsFactors=FALSE)
  application <- apply_content_table(rows,empty_content())
  content <- list(table=empty_content(),conflicts=list(application=application$rows),
    unresolved_reasons=data.frame(Content="Raw Abundance",Code="no_reliable_candidate",
      Reason="legacy rule failed",stringsAsFactors=FALSE))
  empty_stage <- list(table=empty_condition(),status=data.frame(),diagnostics=data.frame())
  ratio <- list(table=empty_ratio(),status=data.frame(),diagnostics=data.frame())

  diagnostic <- auto_regex_failure_diagnostic(rows,content,empty_stage,ratio,"Options")
  raw <- diagnostic$by_content[diagnostic$by_content$Content=="Raw Abundance",]
  expect_identical(raw$RatioReferenceState,"mixed_complete_absent")
  expect_equal(raw$RatioCompleteFamilyCount,1L)
  expect_equal(raw$RatioAbsentFamilyCount,1L)
  expect_equal(raw$RatioPartialFamilyCount,0L)
  expect_true(raw$RatioInferenceRequired)
  expect_false(raw$MissingOrInapplicableReferenceTargets)
  expect_true(raw$GroupedFallbackEligible)
  expect_identical(diagnostic$by_row$RatioReferenceState[1:4],
    c("complete","complete","absent","absent"))
})

test_that("partial ratio references veto grouped fallback with a concrete state", {
  rows <- data.frame(Column=c("alpha-A","alpha-B","beta(1)","beta(2)","other"),
    Content=c(rep("Raw Abundance",4L),"Identifier"), Options="",
    Numerator=c("A","","","",""), Denominator=c("ref","ref","","",""),
    stringsAsFactors=FALSE)
  application <- apply_content_table(rows,empty_content())
  content <- list(table=empty_content(),conflicts=list(application=application$rows),
    unresolved_reasons=data.frame(Content="Raw Abundance",Code="no_reliable_candidate",
      Reason="legacy rule failed",stringsAsFactors=FALSE))
  empty_stage <- list(table=empty_condition(),status=data.frame(),diagnostics=data.frame())
  ratio <- list(table=empty_ratio(),status=data.frame(),diagnostics=data.frame())

  diagnostic <- auto_regex_failure_diagnostic(rows,content,empty_stage,ratio,"Options")
  raw <- diagnostic$by_content[diagnostic$by_content$Content=="Raw Abundance",]
  expect_identical(raw$RatioReferenceState,"partial")
  expect_equal(raw$RatioCompleteFamilyCount,0L)
  expect_equal(raw$RatioAbsentFamilyCount,1L)
  expect_equal(raw$RatioPartialFamilyCount,1L)
  expect_true(raw$MissingOrInapplicableReferenceTargets)
  expect_identical(raw$ActiveVetoes,"MissingOrInapplicableReferenceTargets")
  expect_false(raw$GroupedFallbackEligible)
  expect_identical(diagnostic$by_row$RatioReferenceState[1:2],c("complete","partial"))
})

test_that("one failure log line explains partition recovery for every ratio content", {
  messages <- character()
  auto_regex_infer_rules(fixture("semantic_generality.csv"),
    debug_log=function(message,level) messages <<- c(messages,message))
  labels <- c("Abundance Ratio","Abundance Ratio p-Value",
    "Abundance Ratio adj. p-Value")

  for(label in labels) {
    lines <- messages[startsWith(messages,paste0("Content=",label,";"))]
    expect_length(lines,1L,info=label)
    expect_match(lines,"StructuralFamilyCount=",fixed=TRUE)
    expect_match(lines,"ContentRuleFailure=",fixed=TRUE)
    expect_match(lines,"ConditionRuleFailure=",fixed=TRUE)
    expect_match(lines,"RatioRuleFailure=",fixed=TRUE)
    expect_match(lines,"RatioReferences=complete:",fixed=TRUE)
    expect_match(lines,"missing:",fixed=TRUE)
    expect_match(lines,"partial:",fixed=TRUE)
    expect_match(lines,"SafeSubsetEvidence=",fixed=TRUE)
    expect_match(lines,"ActiveVetoes=",fixed=TRUE)
    expect_match(lines,"PartitionRecovery=",fixed=TRUE)
  }
})

test_that("an exactly explained single-family content label stays on the legacy path", {
  rows <- fixture("content_single_family.csv")
  content <- infer_content(rows)
  condition <- infer_conditions(rows,"Options")
  ratio <- infer_ratios(rows,content_rules=content$table,
    condition_rules=condition$table)
  diagnostic <- auto_regex_failure_diagnostic(rows,content,condition,ratio,"Options")
  raw <- diagnostic$by_content[diagnostic$by_content$Content=="Raw Abundance",]

  expect_true(raw$PositiveRowsExactlyExplained)
  expect_identical(raw$UnexplainedPositiveRows,"")
  expect_false(raw$GroupedFallbackEligible)
  integrated <- auto_regex_infer_rules(rows)
  expect_identical(sum(integrated$rules$table$Content=="Raw Abundance"),1L)
  expect_identical(sum(integrated$rules$condition$Content=="Raw Abundance"),1L)
})

test_that("partition recovery runs only after the unchanged one-rule path fails", {
  calls <- character()
  recovery <- function(metadata,label,condition_target,logger) {
    calls <<- c(calls,label)
    auto_regex_partition_recovery(metadata,label,condition_target,logger)
  }

  classic <- auto_regex_infer_rules(fixture("content_single_family.csv"),
    .partition_recovery=recovery)
  expect_length(calls,0L)
  expect_identical(sum(classic$rules$table$Content=="Raw Abundance"),1L)

  mixed <- auto_regex_infer_rules(fixture("ratio_partition_families.csv"),
    .partition_recovery=recovery)
  expect_identical(calls,"Abundance Ratio")
  expect_gte(sum(mixed$rules$table$Content=="Abundance Ratio"),2L)
})

test_that("typed failures, not human messages, control partition recovery vetoes", {
  rows <- data.frame(Column=c("alpha-A","alpha-B","beta(1)","beta(2)","other"),
    Content=c(rep("Raw Abundance",4L),"Identifier"),Options="",
    stringsAsFactors=FALSE)
  content <- list(table=empty_content(),
    conflicts=list(application=apply_content_table(rows,empty_content())$rows),
    unresolved_reasons=data.frame(Content="Raw Abundance",
      Code=AUTO_REGEX_FAILURE_CODES[["no_reliable_candidate"]],
      Reason="persisted replay wording is informational only",stringsAsFactors=FALSE))
  empty_stage <- list(table=empty_condition(),status=data.frame(),diagnostics=data.frame())

  ordinary <- auto_regex_failure_diagnostic(rows,content,empty_stage,empty_stage,"Options")
  ordinary <- ordinary$by_content[ordinary$by_content$Content=="Raw Abundance",]
  expect_true(ordinary$GroupedFallbackEligible)
  expect_false(ordinary$InvalidPersistedRegexOrReplayInstability)

  content$unresolved_reasons$Code <- AUTO_REGEX_FAILURE_CODES[["replay_instability"]]
  replay <- auto_regex_failure_diagnostic(rows,content,empty_stage,empty_stage,"Options")
  replay <- replay$by_content[replay$by_content$Content=="Raw Abundance",]
  expect_true(replay$InvalidPersistedRegexOrReplayInstability)
  expect_false(replay$GroupedFallbackEligible)

  content$unresolved_reasons$Code <- AUTO_REGEX_FAILURE_CODES[["resource_limit_exhausted"]]
  limited <- auto_regex_failure_diagnostic(rows,content,empty_stage,empty_stage,"Options")
  limited <- limited$by_content[limited$by_content$Content=="Raw Abundance",]
  expect_true(limited$ResourceLimitExhaustion)
  expect_false(limited$GroupedFallbackEligible)
})

test_that("selected exact condition rule is the sole condition failure authority", {
  rows <- data.frame(Column=c("raw_mock_1","raw_control_2"),
    Content="Raw Abundance",Options=c("mock","control"),stringsAsFactors=FALSE)
  content <- infer_content(rows)
  condition <- infer_conditions(rows,"Options")
  ratio <- list(table=empty_ratio(),status=data.frame(),diagnostics=data.frame())
  expect_true("Selected" %in% names(condition$diagnostics))
  expect_true(all(condition$diagnostics$ExactMatch[condition$diagnostics$Selected]))

  result <- auto_regex_failure_diagnostic(rows,content,condition,ratio,"Options")
  failure <- result$by_content[result$by_content$Content=="Raw Abundance",]
  expect_false(failure$ConditionRuleFailure)
})

test_that("ratio header families publish only a complete multi-variant contract", {
  rows <- fixture("ratio_partition_families.csv")
  result <- auto_regex_infer_rules(rows)
  variants <- result$rules$table[
    result$rules$table$Content=="Abundance Ratio",,drop=FALSE]

  expect_gte(nrow(variants),2L)
  expect_identical(anyDuplicated(variants$VariantId),0L)
  expect_true(all(nzchar(variants$VariantId)))
  expect_identical(anyDuplicated(result$rules$table$Priority),0L)
  downstream <- result$rules$ratio[
    result$rules$ratio$Content=="Abundance Ratio",,drop=FALSE]
  expect_identical(nrow(downstream),2L)
  expect_setequal(downstream$VariantId,variants$VariantId)
  replay <- apply_content_table(rows,result$rules$table)
  positive <- rows$Content=="Abundance Ratio"
  expect_true(all(replay$rows$Match[positive]))
  expect_false(any(replay$metadata$Content[!positive]=="Abundance Ratio"))
  search <- result$diagnostics$partition_search
  expect_true(any(search$Authoritative))
  expect_true(all(search$CompleteReplay))
})

test_that("an unreferenced Data-Wizard family does not block a referenced Merged family", {
  rows <- fixture("ratio_partition_families.csv")
  data_wizard <- grepl("_Abundance Ratio$",rows$Column)
  rows$Numerator[data_wizard] <- ""
  rows$Denominator[data_wizard] <- ""
  result <- auto_regex_infer_rules(rows)
  label <- "Abundance Ratio"
  failure <- result$diagnostics$failure_by_content[
    result$diagnostics$failure_by_content$Content==label,,drop=FALSE]

  expect_true(failure$GroupedFallbackEligible)
  expect_identical(failure$RatioReferenceState,"mixed_complete_absent")
  variants <- result$rules$table[result$rules$table$Content==label,,drop=FALSE]
  expect_gte(nrow(variants),2L)
  expect_identical(sum(result$rules$ratio$Content==label),1L)
  replay <- apply_content_table(rows,result$rules$table)
  expect_true(all(replay$metadata$Content[rows$Content==label]==label))
})

test_that("each ratio content has two grammar families independent of biological shape", {
  labels <- c("Abundance Ratio","Abundance Ratio p-Value",
    "Abundance Ratio adj. p-Value")
  merged_markers <- c("Ratio","Pvalue","Qvalue")
  blocks <- lapply(seq_along(labels),function(i) data.frame(
    Column=c(paste0("Merged_",merged_markers[[i]],"_A_vs_B"),
      paste0("Merged_",merged_markers[[i]],"_LongTreatment01_vs_vehicle"),
      paste0("A_B_",labels[[i]]),
      paste0("LongTreatment01_vehicle_",labels[[i]])),
    Content=labels[[i]],Options="Ratio",
    Numerator=c("A","LongTreatment01","",""),
    Denominator=c("B","vehicle","",""),stringsAsFactors=FALSE))
  rows <- rbind(do.call(rbind,blocks),data.frame(Column="Protein IDs",
    Content="Protein ID",Options="",Numerator="",Denominator="",
    stringsAsFactors=FALSE))

  # The references deliberately have different BaseShape/length combinations.
  # Family identity must nevertheless be Merged versus Data Wizard for every
  # content label, with ratio extraction emitted only for referenced Merged.
  signatures <- auto_regex_family_signature(rows,seq_len(nrow(rows)-1L))
  for(i in seq_along(labels)) {
    at <- which(rows$Content==labels[[i]])
    expect_identical(length(unique(signatures[at])),2L,info=labels[[i]])
    result <- auto_regex_infer_rules(rows)
    expect_identical(sum(result$rules$table$Content==labels[[i]]),2L,
      info=labels[[i]])
    expect_identical(sum(result$rules$ratio$Content==labels[[i]]),1L,
      info=labels[[i]])
  }
})

test_that("single ratio header family retains one content and downstream rule", {
  rows <- fixture("ratio_partition_families.csv")
  rows <- rows[!rows$Content=="Abundance Ratio" |
    startsWith(rows$Column,"Merged_"),,drop=FALSE]
  result <- auto_regex_infer_rules(rows)
  label <- "Abundance Ratio"

  expect_identical(sum(result$rules$table$Content==label),1L)
  expect_identical(sum(result$rules$ratio$Content==label),1L)
  expect_identical(result$rules$ratio$VariantId[result$rules$ratio$Content==label],
    result$rules$table$VariantId[result$rules$table$Content==label])
})


test_that("bounded alphabetic Before rules persist and replay unseen carriers", {
  training <- data.frame(
    Column = c("Mica_mock_IFNy_tail", "LongNeutralCarrier_Capsid_IFNy_tail",
      "Mica_AAV2_eGFP_tail", "LongNeutralCarrier_AAV2_eGFP_IFNy_tail"),
    Content = rep("Raw Abundance", 4L),
    Options = c("mock_IFNy", "Capsid_IFNy", "AAV2_eGFP", "AAV2_eGFP_IFNy"),
    stringsAsFactors = FALSE
  )
  spans <- condition_reference_spans(training$Column, training$Options)
  pairs <- condition_stable_boundary_pairs(training$Column, spans)
  expect_true(any(pairs$Before == "[[:alpha:]]{1,30}_"))

  inferred <- infer_conditions(training, "Options")
  expect_identical(nrow(inferred$table), 1L)
  expect_identical(inferred$table$Method, "between")
  expect_identical(inferred$table$Before, "[[:alpha:]]{1,30}_")

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(inferred$table, path)
  persisted <- readRDS(path)
  expect_identical(persisted, inferred$table)

  unseen <- data.frame(
    Column = c("Quartz_mock_IFNy_tail", "Quartz_Capsid_IFNy_tail",
      "Quartz_AAV2_eGFP_tail", "Quartz_AAV2_eGFP_IFNy_tail"),
    Content = rep("Raw Abundance", 4L),
    Options = training$Options,
    stringsAsFactors = FALSE
  )
  extracted <- extract_condition(unseen$Column, persisted$Method,
    persisted$Before, persisted$After, persisted$Separators, persisted$Pos)
  expect_identical(extracted,
    c("mock_IFNy", "Capsid_IFNy", "AAV2_eGFP", "AAV2_eGFP_IFNy"))

  table_replay <- apply_condition_table(unseen, persisted, unseen$Options)
  expect_identical(table_replay$metadata$Options, extracted)
  expect_true(all(table_replay$diagnostics$ExactMatch))

  engine <- create_auto_assign_rule_engine(
    debug_log = function(...) NULL,
    add_processing_log = function(...) NULL,
    rv_table_rules_autoassign_dw = function() data.frame(),
    rv_condition_rules_autoassign_dw = function() data.frame(),
    rv_rules_autoassign_dw = function() data.frame(),
    rules_loaded_centrally = function() FALSE,
    extractedConds_autoassign_dw = function() character()
  )
  auto_replay <- unseen
  auto_replay$Options <- NA_character_
  attr(auto_replay, "variant_id") <- rep(persisted$VariantId,
    nrow(auto_replay))
  setter_value <- NULL
  auto_replay <- engine$apply_condition_autoassign_dw(
    auto_replay, lookup_col = persisted$Content,
    variant_id = persisted$VariantId, position = persisted$Method,
    before = persisted$Before, after = persisted$After,
    phrase_pos = persisted$Pos, sep = persisted$Separators,
    setter = function(value) setter_value <<- value
  )
  expect_identical(auto_replay$Options, extracted)
  expect_identical(setter_value, extracted)
})

test_that("ambiguous alpha boundaries retain a unique observed fallback", {
  sources <- c("Oak_one_Maple_tail", "Willow_two_Cedar_tail")
  options <- c("one", "two")
  spans <- condition_reference_spans(sources, options)
  pairs <- condition_stable_boundary_pairs(sources, spans)

  # The broad policy is a valid representation of the aligned carrier field,
  # but its first ordered match would start at the wrong boundary in these
  # sources.  Canonical replay must therefore reject it rather than suppressing
  # the evidence-specific alternative as well.
  expect_true("[[:alpha:]]{1,30}" %in%
    condition_outside_field_candidates(matrix(c("Oak", "Willow"), ncol = 1L))$Pattern)
  expect_false(any(pairs$Before == "[[:alpha:]]{1,30}_"))
  expect_true(any(pairs$Before == "(?:Oak|Willow)_"))
  specific <- pairs[pairs$Before == "(?:Oak|Willow)_", , drop = FALSE]
  expect_identical(nrow(specific), 1L)
  expect_identical(extract_condition(sources, specific$Method,
    specific$Before, specific$After, "", 1L), options)
})

test_that("condition-boundary regressions select and replay only stable canonical rules", {
  engine <- create_auto_assign_rule_engine(
    debug_log = function(...) NULL,
    add_processing_log = function(...) NULL,
    rv_table_rules_autoassign_dw = function() data.frame(),
    rv_condition_rules_autoassign_dw = function() data.frame(),
    rv_rules_autoassign_dw = function() data.frame(),
    rules_loaded_centrally = function() FALSE,
    extractedConds_autoassign_dw = function() character()
  )
  # These carrier names are deliberately arbitrary alphabetic identifiers of
  # different lengths.  "proteome" and "secretome" also prove that otherwise
  # identical separators do not turn either observed value into vocabulary.
  canonical <- data.frame(
    Column = c("A_mock_IFNy_tail", "Birch_Capsid_IFNy_tail",
      "LongCarrier_AAV2_eGFP_tail", "proteome_AAV2_eGFP_IFNy_tail",
      "secretome_mock_IFNy_tail"),
    Content = rep("Raw Abundance", 5L),
    Options = c("mock_IFNy", "Capsid_IFNy", "AAV2_eGFP",
      "AAV2_eGFP_IFNy", "mock_IFNy"), stringsAsFactors = FALSE)
  carriers <- sub("_.*$", "", canonical$Column)
  expect_gte(length(unique(carriers)), 3L)
  expect_gt(length(unique(nchar(carriers))), 2L)
  expect_setequal(unique(canonical$Options), c("mock_IFNy", "Capsid_IFNy",
    "AAV2_eGFP", "AAV2_eGFP_IFNy"))

  # This is the SCR528/SCR529 reproduction verbatim, including U+00A0 (not an
  # ASCII space) between `1` and `_2` in the second source.
  scr528_scr529 <- data.frame(
    Column = c(
      "SCR529_X002_secretome_mock_1\u00A0_2_1_7559.d.PG.Quantity",
      "SCR529_X006_secretome_mock_IFNy_1\u00A0_6_1_7563.d.PG.Quantity",
      "SCR529_X018_secretome_AAV2_eGFP_1\u00A0_18_1_7575.d.PG.Quantity",
      "SCR529_X022_secretome_AAV2_eGFP_IFNy_1_22_1_7579.d.PG.Quantity",
      "SCR528_X006_proteome_mock_IFNy_1_6_1_7534.d.PG.Quantity"),
    Content = rep("Raw Abundance", 5L),
    Options = c("mock", "mock_IFNy", "AAV2_eGFP", "AAV2_eGFP_IFNy",
      "mock_IFNy"), stringsAsFactors = FALSE)
  expect_true(all(grepl("1\u00A0_", scr528_scr529$Column[1:3], fixed = TRUE)))
  expect_false(any(grepl("\u00A0", scr528_scr529$Column[4:5], fixed = TRUE)))
  expect_identical(utf8ToInt(sub("^.*_1(.{1})_[[:digit:]]+_1_.*$", "\\1",
    scr528_scr529$Column[[1L]], perl = TRUE)), 160L)

  reproductions <- list(
    arbitrary_alpha_carriers = canonical,
    scr528_scr529_nbsp = scr528_scr529
  )

  for (case_name in names(reproductions)) {
    metadata <- reproductions[[case_name]]
    inferred <- infer_conditions(metadata, "Options")
    expect_identical(nrow(inferred$table), 1L, info = case_name)
    rule <- unserialize(serialize(inferred$table[1L, , drop = FALSE], NULL))
    expect_identical(names(rule), CONDITION_FIELDS, info = case_name)

    expected <- extract_condition(metadata$Column, rule$Method, rule$Before,
      rule$After, rule$Separators, rule$Pos)
    expect_identical(expected, metadata$Options, info = case_name)
    if (identical(rule$Method, "between")) {
      # The selected terminator begins strictly after each complete reference;
      # it must not consume an internal underscore in a composite condition.
      expect_false(any(vapply(seq_len(nrow(metadata)), function(i)
        grepl(paste0("(?-i:", rule$After, ")"), metadata$Options[[i]],
          perl = TRUE), logical(1))), info = case_name)
    }
    table_replay <- apply_condition_table(metadata, rule, metadata$Options)
    expect_identical(table_replay$metadata$Options, metadata$Options,
      info = case_name)
    expect_true(all(table_replay$diagnostics$ExactMatch), info = case_name)
    replay <- metadata
    replay$Options <- NA_character_
    attr(replay, "variant_id") <- rep(rule$VariantId, nrow(replay))
    setter_value <- NULL
    replay <- engine$apply_condition_autoassign_dw(
      replay, lookup_col = rule$Content, variant_id = rule$VariantId,
      position = rule$Method, before = rule$Before, after = rule$After,
      phrase_pos = rule$Pos, sep = rule$Separators,
      setter = function(value) setter_value <<- value
    )

    expect_identical(replay$Options, expected, info = case_name)
    expect_identical(setter_value, expected, info = case_name)
    expect_identical(attr(replay, "variant_id"),
      rep(rule$VariantId, nrow(replay)), info = case_name)
    expect_identical(attr(replay, "condition_rule_status")$status,
      "applied", info = case_name)
  }

  # A generalized alpha boundary that occurs twice cannot identify one stable
  # ordered pair, even though each occurrence has the same separator shape.
  repeated_sources <- c("Oak_one_Oak_tail", "Willow_two_Willow_tail")
  repeated_spans <- condition_reference_spans(repeated_sources, c("one", "two"))
  expect_identical(nrow(condition_stable_boundary_pairs(repeated_sources,
    repeated_spans)), 0L)

  # Nor may an alpha-shaped boundary match inside (or exactly terminate) the
  # complete expected condition.  Internal underscores belong to the reference.
  embedded_sources <- c("Oak_mock_Oak_tail", "Willow_Capsid_Willow_tail")
  embedded_options <- c("mock_Oak", "Capsid_Willow")
  embedded_spans <- condition_reference_spans(embedded_sources, embedded_options)
  embedded_pairs <- condition_stable_boundary_pairs(embedded_sources,
    embedded_spans)
  expect_identical(nrow(embedded_pairs), 0L)
  expect_identical(mapply(substr, embedded_sources[embedded_spans$Row],
    embedded_spans$Start, embedded_spans$End, USE.NAMES = FALSE),
    embedded_options)
})
