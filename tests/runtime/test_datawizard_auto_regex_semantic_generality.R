library(testthat)

source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")

generality_fixture <- function() read.csv(
  "tests/fixtures/regex_metadata_assistant/semantic_generality.csv",
  stringsAsFactors=FALSE, na.strings="NA", check.names=FALSE)

position_ratio_rule <- function(content="Abundance Ratio") data.frame(
  Content=content, Method="Position in String", Separators="\\s+|\\(|\\)|/",
  Invert=FALSE, NumBefore=NA_character_, NumAfter=NA_character_,
  DenBefore=NA_character_, DenAfter=NA_character_, NumPos=3L, DenPos=4L,
  stringsAsFactors=FALSE, check.names=FALSE)

content_metric <- function(rule, rows, label) score_pattern(
  rule$Include, rows$Column, rows$Content==label, rule$Exclude)

test_that("ratio semantics generalize portable values but retain class literals", {
  rows <- generality_fixture()
  result <- auto_regex_infer_rules(rows, condition_target="Options")
  expect_length(result$errors, 0L)

  ordinary <- result$rules$table[result$rules$table$Content=="Abundance Ratio",,drop=FALSE]
  pvalue <- result$rules$table[result$rules$table$Content=="Abundance Ratio p-Value",,drop=FALSE]
  adjusted <- result$rules$table[result$rules$table$Content=="Abundance Ratio adj. p-Value",,drop=FALSE]
  expect_equal(nrow(ordinary),1L); expect_equal(nrow(pvalue),1L); expect_equal(nrow(adjusted),1L)
  expect_false(grepl("ERU",ordinary$Include,fixed=TRUE))
  expect_false(grepl("(?<![[:alnum:]])C(?![[:alnum:]])",ordinary$Include,perl=TRUE))
  expect_true(grepl("Value",pvalue$Include,fixed=TRUE))
  expect_true(grepl("Adj",adjusted$Include,fixed=TRUE))
  expect_true(grepl("Value",adjusted$Include,fixed=TRUE))

  lineage <- result$diagnostics$content_refinement_lineage
  expect_true(lineage$Accepted[lineage$Content=="Abundance Ratio"])
  expect_true(grepl("ERU",lineage$RemovedValues[lineage$Content=="Abundance Ratio"],fixed=TRUE))
  expect_true(grepl("Control_2",lineage$RemovedValues[lineage$Content=="Abundance Ratio"],fixed=TRUE))
  expect_true(all(vapply(c(ordinary$Include,pvalue$Include,adjusted$Include),
    function(pattern) validate_pcre(regex_from_miraprot_storage(pattern,"content"))$valid,
    logical(1))))
  condition_replay <- result$diagnostics$condition_replay
  applicable_conditions <- !is.na(condition_replay$ReferenceAvailable) &
    condition_replay$ReferenceAvailable
  expect_true(all(condition_replay$ExactMatch[applicable_conditions]))
  ratio_replay <- result$diagnostics$ratio_replay
  expect_true(all(ratio_replay$Success[!is.na(ratio_replay$Success)]))
})

test_that("different lengths, repeats, punctuation, ambiguity, and Unicode offsets are safe", {
  rows <- data.frame(
    Column=c("(ERU) / (C)","(Treatment-01) / (Control_2)",
      "(Long treatment + rescue) / (vehicle.control)","(C) / (C)",
      "prefix ERU / C suffix"),
    Content=rep("Abundance Ratio",5L), Options="Ratio",
    Numerator=c("ERU","Treatment-01","Long treatment + rescue","C","ERU"),
    Denominator=c("C","Control_2","vehicle.control","C","C"),
    stringsAsFactors=FALSE,check.names=FALSE)
  rules <- position_ratio_rule()
  rules$NumPos <- 1L; rules$DenPos <- 2L
  spans <- auto_regex_semantic_spans(rows,empty_condition(),data.frame(),rules,data.frame())

  expect_true(all(spans$SafeToGeneralize[spans$Row %in% 1:3]))
  expect_false(any(spans$SafeToGeneralize[spans$Row %in% 4:5]))
  repeated <- spans[spans$Row==4L,,drop=FALSE]
  expect_true(all(grepl("more than once",repeated$Diagnostic,fixed=TRUE)))
  unicode_source <- "😀 Ratio (α β+) / (Control_2)"
  unicode <- data.frame(Start=c(10L,19L),End=c(13L,27L),
    Semantic=c("numerator","denominator"),Reference=c("α β+","Control_2"),
    ReplacementAtom=c("[^()]+","[^()]+"),SafeToGeneralize=TRUE,
    stringsAsFactors=FALSE,check.names=FALSE)
  unicode_template <- auto_regex_source_template(unicode_source,unicode)
  expect_identical(mapply(substr,MoreArgs=list(x=unicode_source),unicode$Start,
    unicode$End,USE.NAMES=FALSE),unicode$Reference)
  expect_identical(unicode_template$nodes$Text[unicode_template$nodes$Type=="semantic"],
    unicode$Reference)

  template_spans <- spans[spans$Row==2L,,drop=FALSE]
  template_spans$Row <- 1L
  template <- auto_regex_source_template(rows$Column[[2L]],template_spans)
  expect_identical(sum(template$nodes$Type=="semantic"),2L)
  overlap <- template_spans; overlap$Start[[2L]] <- overlap$End[[1L]]
  expect_error(auto_regex_source_template(rows$Column[[2L]],overlap),"overlap")
})

test_that("include/exclude selection uses the simplest direct discriminator", {
  rows <- data.frame(Column=c("Abundance Ratio A","Abundance Ratio B",
      "Abundance Ratio P-Value A","Abundance Ratio P-Value B",
      "Abundance Ratio Adj. P-Value A","Abundance Ratio Adj. P-Value B"),
    Content=rep(c("Abundance Ratio","Abundance Ratio p-Value",
      "Abundance Ratio adj. p-Value"),each=2L),
    stringsAsFactors=FALSE,check.names=FALSE)
  inferred <- infer_content(rows)
  ordinary <- inferred$table[inferred$table$Content=="Abundance Ratio",,drop=FALSE]
  pvalue <- inferred$table[inferred$table$Content=="Abundance Ratio p-Value",,drop=FALSE]
  adjusted <- inferred$table[inferred$table$Content=="Abundance Ratio adj. p-Value",,drop=FALSE]
  expect_false(safe_grepl(ordinary$Include,"Protein P-Value"))
  expect_false(safe_grepl(pvalue$Include,"Abundance Ratio A"))
  expect_false(safe_grepl(pvalue$Include,"Abundance Ratio Adj. P-Value A") &&
    (!nzchar(pvalue$Exclude) || !safe_grepl(pvalue$Exclude,"Abundance Ratio Adj. P-Value A")))
  expect_true(safe_grepl(adjusted$Include,"Abundance Ratio Adj. P-Value A"))
  expect_true(nchar(adjusted$Include) <= nchar("Abundance Ratio") + nchar("Adj. P-Value") + 20L)
})

test_that("refinement preserves replay, conflicts, unaffected bytes, and literal fallback", {
  rows <- generality_fixture()
  first <- infer_content(rows)
  ratios <- infer_ratios(rows,content_rules=first$table,
    condition_rules=empty_condition())
  spans <- auto_regex_semantic_spans(rows,empty_condition(),data.frame(),ratios$table,
    ratios$diagnostics)
  before <- serialize(first$table[!first$table$Content %in% unique(spans$Content),,drop=FALSE],NULL)
  refined <- refine_content_with_semantic_spans(rows,first$table,spans)
  after <- serialize(refined$table[!refined$table$Content %in% unique(spans$Content),,drop=FALSE],NULL)
  expect_identical(after,before)

  for (label in unique(rows$Content)) {
    old <- first$table[first$table$Content==label,,drop=FALSE]
    new <- refined$table[refined$table$Content==label,,drop=FALSE]
    if(nrow(old)&&nrow(new)) {
      old_metric <- content_metric(old,rows,label); new_metric <- content_metric(new,rows,label)
      expect_lte(new_metric$FP,old_metric$FP)
      expect_lte(new_metric$FN,old_metric$FN)
    }
  }
  expect_lte(nrow(refined$application$conflicts),nrow(apply_content_table(rows,first$table)$conflicts))

  unsafe_rows <- data.frame(Column=c("Measure (A)","Measure (B)","Measure (noise)"),
    Content=c("Target","Target","Negative"),stringsAsFactors=FALSE,check.names=FALSE)
  unsafe_rules <- data.frame(Content=c("Target","Negative"),
    Include=c("^Measure \\(A\\)$","noise"),Exclude="",Transformation=NA_character_,
    stringsAsFactors=FALSE,check.names=FALSE)
  unsafe_spans <- do.call(rbind,lapply(1:2,function(i)data.frame(Row=i,Content="Target",
    Semantic="condition",Reference=substr(unsafe_rows$Column[[i]],10L,10L),Start=10L,End=10L,
    ReplacementAtom="[^()]+",SafeToGeneralize=TRUE,stringsAsFactors=FALSE)))
  rejected <- refine_content_with_semantic_spans(unsafe_rows,unsafe_rules,unsafe_spans)
  expect_identical(rejected$table$Include[[1L]],unsafe_rules$Include[[1L]])
})

test_that("one-row literal positives are diagnostic suggestions, not authoritative rules", {
  singleton <- data.frame(Column=c("Unique quantitative signal","Other metadata"),
    Content=c("Raw Abundance","Metadata"),stringsAsFactors=FALSE,check.names=FALSE)
  inferred <- infer_content(singleton)
  expect_false("Raw Abundance" %in% inferred$table$Content)
  metric <- inferred$metrics[inferred$metrics$Content=="Raw Abundance",,drop=FALSE]
  expect_identical(as.integer(metric[1L,c("TP","FP","FN")]),c(0L,0L,1L))
  suggestions <- inferred$candidate_evidence[
    inferred$candidate_evidence$Content=="Raw Abundance",,drop=FALSE]
  expect_true(nrow(suggestions)>0L)
  expect_false(any(suggestions$Authoritative))
  expect_true(any(suggestions$CandidateFamily=="whole_header_literal"))
  expect_true(all(!suggestions$EvidenceThresholdPassed))
  expect_true(all(c("SupportingSourceRows","DistinctSourceNames","CoveredExamples",
    "UncoveredExamples","FalsePositiveCount","DownstreamExactMatchCount",
    "ValidWithoutWholeHeaderMemorization") %in% names(suggestions)))

  accepted <- generality_fixture()
  accepted_result <- infer_content(accepted)
  expect_true("Identifier" %in% accepted_result$table$Content)
  rejected <- data.frame(Column=c("Gene ID","subject id"),
    Content=c("Identifier","Metadata"),stringsAsFactors=FALSE,check.names=FALSE)
  expect_false("Identifier" %in% infer_content(rejected)$table$Content)

  messages <- character()
  reliable <- data.frame(Column=c("Protein accession","Protein description"),
    Content=c("Identifier","Metadata"),stringsAsFactors=FALSE,check.names=FALSE)
  ordinary <- infer_content(reliable,logger=function(level,component,step,message,...)
    messages <<- c(messages,message))
  expect_true("Identifier" %in% ordinary$table$Content)
  expect_false(any(grepl("Identifier fallback",messages,fixed=TRUE)))
})

test_that("schema, load contract, timing, candidate, diagnostics, and logging stay bounded", {
  rows <- generality_fixture()
  quiet <- auto_regex_infer_rules(rows,debug_log=function(message,level) NULL)
  noisy <- auto_regex_infer_rules(rows,debug_log=function(message,level)
    invisible(paste(level,message)))
  expect_identical(quiet$rules,noisy$rules)
  expect_identical(names(quiet$rules$table),CONTENT_FIELDS)
  expect_identical(names(quiet$rules$condition),CONDITION_FIELDS)
  expect_identical(names(quiet$rules$ratio),RATIO_FIELDS)
  expect_type(quiet$rules$table$Include,"character")
  expect_type(quiet$rules$ratio$Invert,"logical")
  expect_length(validate_export(quiet$rules,rows),0L)
  path <- tempfile(fileext=".rds")
  on.exit(unlink(path),add=TRUE)
  saveRDS(quiet$rules,path)
  loaded <- readRDS(path)
  expect_identical(loaded,quiet$rules)
  expect_length(validate_export(loaded,rows),0L)
  expect_named(quiet$timings,c("preprocessing","content","condition","ratio",
    "prerequisite_application","candidate_construction","candidate_scoring",
    "completion_gate_replay","semantic_span_analysis","targeted_content_refinement",
    "downstream_replay","payload_validation","total"))
  expect_true(all(is.finite(quiet$timings) & quiet$timings>=0))
  expect_lt(quiet$timings[["content"]],30000)
  expect_lt(quiet$timings[["semantic_span_analysis"]],30000)
  expect_lt(quiet$timings[["targeted_content_refinement"]],30000)
  expect_lt(quiet$timings[["total"]],60000)

  candidate_rows <- rep("(A) / (B)",200L)
  span_rows <- do.call(rbind,lapply(seq_along(candidate_rows),function(i) data.frame(
    Source=i,Start=c(2L,8L),End=c(2L,8L),Semantic=c("numerator","denominator"),
    Reference=c("A","B"),ReplacementAtom="[^()]+",SafeToGeneralize=TRUE,
    stringsAsFactors=FALSE,check.names=FALSE)))
  generated <- auto_regex_generalized_content_candidates(candidate_rows,span_rows)
  expect_lte(nrow(generated$includes),GENERALIZED_INCLUDE_LIMIT)
  expect_lte(nrow(generated$exclusions),GENERALIZED_EXCLUDE_LIMIT)
  expect_lte(nrow(generated$combinations),GENERALIZED_COMBINATION_LIMIT)

  large_metadata <- rows[rep(seq_len(nrow(rows)),length.out=2000L),,drop=FALSE]
  large_diagnostics <- apply_content_table(large_metadata,quiet$rules$table)$rows
  bounded <- auto_regex_bound_diagnostics(large_diagnostics)
  expect_identical(nrow(bounded),AUTO_REGEX_DIAGNOSTIC_ROW_LIMIT)
  expect_identical(bounded$Row,seq_len(AUTO_REGEX_DIAGNOSTIC_ROW_LIMIT))
})
