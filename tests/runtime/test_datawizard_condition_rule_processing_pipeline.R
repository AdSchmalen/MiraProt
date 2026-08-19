#!/usr/bin/env Rscript

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R")
source("modules/Data Wizard/assign rules/datawizard_assign_rules_utils.R")
source("modules/Data Wizard/datawizard_utils.R")

`%>%` <- dplyr::`%>%`

engine <- create_auto_assign_rule_engine(
  debug_log = function(...) NULL,
  add_processing_log = function(...) NULL,
  rv_table_rules_autoassign_dw = function() data.frame(),
  rv_condition_rules_autoassign_dw = function() data.frame(),
  rv_rules_autoassign_dw = function() data.frame(),
  rules_loaded_centrally = function() FALSE,
  extractedConds_autoassign_dw = function() character()
)

# These are the four rule shapes emitted by condition inference: two boundary
# rules and the positional rule, in addition to a fully bounded rule.
inferred_rules <- data.frame(
  Content = c("Raw Abundance", "Normalized Abundance", "Found in Sample", "# PSMs"),
  Method = c("between", "start", "end", "phrase_position"),
  Before = c("::", "", "::", ""),
  After = c("::", "::", "", ""),
  Separators = c("", "", "", ","),
  Pos = c(1L, 1L, 1L, 2L),
  stringsAsFactors = FALSE
)

columns <- list(
  c("run1::mock::signal", "run2::treated::signal"),
  c("mock::run1", "treated::run2"),
  c("run1::mock", "run2::treated"),
  c("run1,mock,signal", "run2,treated,signal")
)
expected_conditions <- rep(list(c("mock", "treated")), 4L)

metadata <- data.frame(Column = character(), Content = character(),
                       Options = character(), Sample = character())
for (i in seq_len(nrow(inferred_rules))) {
  rule <- inferred_rules[i, ]
  block <- data.frame(Column = columns[[i]], Content = rule$Content,
                      Options = NA_character_, Sample = NA_character_)
  applied <- engine$apply_condition_rule(
    block, lookup_col = rule$Content, position = rule$Method,
    before = rule$Before, after = rule$After, phrase_pos = rule$Pos,
    sep = rule$Separators, setter = function(...) NULL
  )
  stopifnot(
    identical(applied$Options, expected_conditions[[i]]),
    all(is.na(applied$Sample)),
    identical(attr(applied, "condition_rule_status")$status, "applied")
  )
  metadata <- rbind(metadata, as.data.frame(applied))
}
metadata <- engine$finalize_condition_sample_names(metadata)
stopifnot(all(!is.na(metadata$Sample)), all(nzchar(metadata$Sample)))

# Pure downstream processing fills missing samples only. Names already made
# valid and unique by the shared builder must survive both stages byte-for-byte.
valid_names <- metadata$Sample
processed <- process_dataframe(metadata)
postprocessed <- postprocess_dataframe(processed)
stopifnot(identical(processed$Sample, valid_names),
          identical(postprocessed$Sample, valid_names))

# Post-processing must consume the same canonical classifier as Auto Regex.
# Exercise the complete metadata Content vocabulary so additions to the
# sample-bearing allowlist cannot be silently cleared by a downstream copy.
content_vocabulary <- datawizard_metadata_content_choices(include_blank = FALSE)
classification_input <- data.frame(
  Content = content_vocabulary,
  Options = NA_character_,
  Sample = paste0("sample-", seq_along(content_vocabulary)),
  stringsAsFactors = FALSE
)
classification_result <- postprocess_dataframe(classification_input)
sample_bearing <- is_sample_bearing_content(content_vocabulary)
stopifnot(
  all(SAMPLE_BEARING_CONTENT_TYPES %in% content_vocabulary),
  any(sample_bearing),
  any(!sample_bearing),
  identical(
    classification_result$Sample[sample_bearing],
    classification_input$Sample[sample_bearing]
  ),
  all(is.na(classification_result$Sample[!sample_bearing]))
)

# Sample processing must skip metadata which cannot need a generated name,
# retain every row in an affected group as structural evidence, and mutate only
# missing sample-bearing rows. Instrument the expensive group-level builder so
# this is a runtime assertion rather than just an output assertion.
original_process_sample <- process_sample
process_sample_calls <- list()
process_sample <- function(strings, options) {
  process_sample_calls[[length(process_sample_calls) + 1L]] <<- strings
  paste0("generated-", seq_along(strings))
}

run_sample_case <- function(input, expected_calls, expected_sample,
                            expected_evidence = NULL) {
  process_sample_calls <<- list()
  result <- process_dataframe(input)
  stopifnot(
    identical(length(process_sample_calls), expected_calls),
    identical(result$Sample, expected_sample)
  )
  if (!is.null(expected_evidence)) {
    stopifnot(identical(process_sample_calls[[1L]], expected_evidence))
  }
  invisible(result)
}

# 1. A zero-row frame returns without invoking the builder.
run_sample_case(
  data.frame(Column = character(), Content = character(), Options = character(),
             Sample = character()),
  0L, character()
)

# 2. A populated metadata skeleton with no assigned Content is ineligible.
run_sample_case(
  data.frame(Column = c("run-a", "run-b"), Content = c("", ""),
             Options = NA_character_, Sample = NA_character_),
  0L, rep(NA_character_, 2L)
)

# 3. Identifier, Description, and Ratio metadata never carry sample names.
run_sample_case(
  data.frame(Column = c("accession", "description", "ratio"),
             Content = c("Identifier", "Description", "Ratio"),
             Options = NA_character_, Sample = NA_character_),
  0L, rep(NA_character_, 3L)
)

# 4. Complete sample-bearing rows require no regeneration.
run_sample_case(
  data.frame(Column = c("raw-a", "raw-b"), Content = "Raw Abundance",
             Options = "condition", Sample = c("kept-a", "kept-b")),
  0L, c("kept-a", "kept-b")
)

# 5. A mixed group is built once, but only its missing row is written.
run_sample_case(
  data.frame(Column = c("raw-a", "raw-b"), Content = "Raw Abundance",
             Options = "condition", Sample = c("curated", NA_character_)),
  1L, c("curated", "generated-2"), c("raw-a", "raw-b")
)

# 6. Whitespace-only names are missing and are replaced in one group call.
run_sample_case(
  data.frame(Column = c("raw-a", "raw-b"), Content = "Raw Abundance",
             Options = "condition", Sample = c("named", " \t ")),
  1L, c("named", "generated-2"), c("raw-a", "raw-b")
)

# 7. Even when a condition rule targets it, non-sample-bearing Content does
# not cause the downstream sample builder to run or mutate Sample.
non_sample_rule_result <- engine$apply_condition_rule(
  data.frame(Column = "run1::control::description", Content = "Description",
             Options = NA_character_, Sample = NA_character_),
  lookup_col = "Description", position = "between", before = "::", after = "::",
  setter = function(...) NULL
)
run_sample_case(non_sample_rule_result, 0L, NA_character_)
stopifnot(identical(non_sample_rule_result$Options, "control"))

# 8. One missing row triggers exactly one call containing all rows in its group;
# the populated row is structural evidence and remains byte-for-byte intact.
run_sample_case(
  data.frame(
    Column = c("batch::control::signal", "batch::treated::signal", NA),
    Content = "Normalized Abundance", Options = c("condition", "condition", "ignored"),
    Sample = c("curated-evidence", NA_character_, NA_character_)
  ),
  1L, c("curated-evidence", "generated-2", NA_character_),
  c("batch::control::signal", "batch::treated::signal")
)
process_sample <- original_process_sample

# The orchestration eligibility gate only requests processing for missing or
# whitespace-only samples on Content rows that can actually carry samples.
eligibility_input <- data.frame(
  Column = c("raw-a", "raw-b", "identifier"),
  Content = c("Raw Abundance", "Raw Abundance", "Accession"),
  Options = NA_character_,
  Sample = c("curated", "   ", NA_character_),
  stringsAsFactors = FALSE
)
stopifnot(
  metadata_needs_sample_processing(eligibility_input),
  !metadata_needs_sample_processing(eligibility_input[-2L, , drop = FALSE]),
  metadata_needs_sample_processing(eligibility_input[, setdiff(names(eligibility_input), "Sample")])
)

# Condition extraction and sample generation have independent gates. A
# non-sample-bearing row still receives its condition, while an existing sample
# name suppresses generation without suppressing condition assignment.
original_builder <- build_unique_sample_names
builder_calls <- 0L
build_unique_sample_names <- function(...) {
  builder_calls <<- builder_calls + 1L
  original_builder(...)
}

non_sample <- engine$apply_condition_rule(
  data.frame(Column = "run1::control::count", Content = "# PSMs",
             Options = NA_character_, Sample = NA_character_),
  lookup_col = "# PSMs", position = "between", before = "::", after = "::",
  setter = function(...) NULL
)
existing_sample <- engine$apply_condition_rule(
  data.frame(Column = "run1::control::signal", Content = "Raw Abundance",
             Options = NA_character_, Sample = "curated-name"),
  lookup_col = "Raw Abundance", position = "between", before = "::", after = "::",
  setter = function(...) NULL
)
stopifnot(
  identical(non_sample$Options, "control"),
  is.na(non_sample$Sample),
  identical(existing_sample$Options, "control"),
  identical(existing_sample$Sample, "curated-name"),
  identical(builder_calls, 0L)
)

# Within a matching sample-bearing group, each accepted condition rule replaces
# Options on every matched row and sends that same complete vector to its setter.
setter_conditions <- NULL
partially_named <- engine$apply_condition_rule(
  data.frame(
    Column = c("run1::control::signal", "run2::treated::signal"),
    Content = rep("Raw Abundance", 2L),
    Options = c("stale-control", "stale-treated"),
    Sample = c("curated-name", NA_character_)
  ),
  lookup_col = "Raw Abundance", position = "between", before = "::", after = "::",
  setter = function(value) setter_conditions <<- value
)
stopifnot(
  identical(builder_calls, 0L),
  identical(partially_named$Options, c("control", "treated")),
  identical(setter_conditions, partially_named$Options),
  identical(partially_named$Sample[1L], "curated-name"),
  is.na(partially_named$Sample[2L])
)
partially_named <- engine$finalize_condition_sample_names(partially_named)
stopifnot(
  identical(builder_calls, 1L),
  identical(partially_named$Sample[1L], "curated-name"),
  !is.na(partially_named$Sample[2L]), nzchar(partially_named$Sample[2L])
)
build_unique_sample_names <- original_builder

# Condition rules are applied in table order, including when multiple rules
# target the same Content.  The broad rule deliberately produces each complete
# column name (an incorrect, nonempty result); the later bounded rule must
# replace it on every matching row rather than preserving the first result.
overlapping_conditions <- c(
  "mock", "mock_IFNy", "Capsid", "Capsid_IFNy", "AAV2_eGFP",
  "AAV2_eGFP_IFNy"
)
overlapping_columns <- paste0("Intensity [", overlapping_conditions, "]")
overlapping_rules <- data.frame(
  Content = rep("Raw Abundance", 2L),
  Method = c("whole", "between"),
  Before = c("", "\\["),
  After = c("", "\\]"),
  Separators = c("", ""),
  Pos = c(1L, 1L),
  stringsAsFactors = FALSE
)
overlapping_engine <- create_auto_assign_rule_engine(
  debug_log = function(...) NULL,
  add_processing_log = function(...) NULL,
  rv_table_rules_autoassign_dw = function() data.frame(),
  rv_condition_rules_autoassign_dw = function() overlapping_rules,
  rv_rules_autoassign_dw = function() data.frame(),
  rules_loaded_centrally = function() FALSE,
  extractedConds_autoassign_dw = function() character()
)
overlapping_result <- overlapping_engine$apply_auto_assign_rules(data.frame(
  Column = overlapping_columns,
  Content = rep("Raw Abundance", length(overlapping_conditions)),
  Options = NA_character_,
  Sample = NA_character_,
  stringsAsFactors = FALSE
))
stopifnot(
  identical(overlapping_result$Options, overlapping_conditions),
  all(overlapping_result$Options != overlapping_result$Column),
  identical(sort(unique(overlapping_result$Options)), sort(overlapping_conditions))
)

# Pipeline finalization is a distinct phase after the complete condition-rule
# loop, so it must also run when that loop has zero rules. It uses the final
# Options vector as evidence, preserves curated names, and cannot mutate any
# other assignment columns.
pipeline_debug <- character()
pipeline_engine <- create_auto_assign_rule_engine(
  debug_log = function(message, ...) pipeline_debug <<- c(pipeline_debug, message),
  add_processing_log = function(...) NULL,
  rv_table_rules_autoassign_dw = function() data.frame(),
  rv_condition_rules_autoassign_dw = function() data.frame(),
  rv_rules_autoassign_dw = function() data.frame(),
  rules_loaded_centrally = function() TRUE,
  extractedConds_autoassign_dw = function() character()
)
pipeline_input <- data.frame(
  Column = c("batch::control::A", "batch::treated::B", "accession"),
  Content = c("Raw Abundance", "Raw Abundance", "Identifier"),
  Options = c("control", "treated", "protein-id"),
  Sample = c("curated-name", " \t", NA_character_),
  Numerator = c("keep-num-1", "keep-num-2", "keep-num-3"),
  Denominator = c("keep-den-1", "keep-den-2", "keep-den-3"),
  Transformation = c("log2", "log2", "none"),
  stringsAsFactors = FALSE
)
pipeline_result <- pipeline_engine$apply_auto_assign_rules(pipeline_input)
immutable_columns <- c("Options", "Content", "Numerator", "Denominator", "Transformation")
pipeline_diagnostics <- attr(pipeline_result, "sample_name_diagnostics", exact = TRUE)
stopifnot(
  identical(pipeline_result$Sample[[1L]], pipeline_input$Sample[[1L]]),
  !is.na(pipeline_result$Sample[[2L]]), nzchar(trimws(pipeline_result$Sample[[2L]])),
  is.na(pipeline_result$Sample[[3L]]),
  identical(pipeline_result[immutable_columns], pipeline_input[immutable_columns]),
  is.data.frame(pipeline_diagnostics),
  nrow(pipeline_diagnostics) == 2L,
  identical(pipeline_diagnostics$extracted_condition, pipeline_input$Options[1:2]),
  identical(pipeline_diagnostics$preserved_existing, c(TRUE, FALSE)),
  any(grepl("Sample-name diagnostics attached", pipeline_debug, fixed = TRUE))
)

# The complete group informs invariant removal, while an exact collision with a
# preserved name refines only the generated value.
collision_logs <- character()
collision_engine <- create_auto_assign_rule_engine(
  debug_log = function(...) NULL,
  add_processing_log = function(step, status, message) {
    if (identical(status, "warning")) collision_logs <<- c(collision_logs, message)
  },
  rv_table_rules_autoassign_dw = function() data.frame(),
  rv_condition_rules_autoassign_dw = function() data.frame(),
  rv_rules_autoassign_dw = function() data.frame(),
  rules_loaded_centrally = function() FALSE,
  extractedConds_autoassign_dw = function() character()
)
collision_result <- collision_engine$apply_condition_rule(
  data.frame(
    Column = c("batch::control::A", "batch::treated::B"),
    Content = rep("Raw Abundance", 2L),
    Options = c("control", "treated"),
    Sample = c("B_treated", "")
  ),
  lookup_col = "Raw Abundance", position = "between", before = "::", after = "::",
  setter = function(...) NULL
)
collision_result <- collision_engine$finalize_condition_sample_names(collision_result)
stopifnot(
  identical(collision_result$Sample, c("B_treated", "B_treated_2")),
  length(collision_logs) == 1L,
  grepl("collision with a preserved name", collision_logs[[1L]], fixed = TRUE)
)

cat("condition rule processing pipeline regression checks passed\n")
