# Slow, opt-in production-shape runtime guard. Run after the ordinary golden
# tests; it also verifies that performance work did not alter persisted rules.
library(testthat)
source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R")
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R")

fixture <- read.csv("tests/fixtures/regex_metadata_assistant/full_110_case.csv",
  stringsAsFactors=FALSE,na.strings="NA",check.names=FALSE)
fixture[] <- lapply(fixture,function(x) { if(is.character(x))x[is.na(x)]<-"";x })

# These budgets intentionally sit below the hard abstention limits. They are a
# regression alarm, never permission to expand search resources.
TOTAL_BUDGET_SECONDS <- 30
PER_LABEL_BUDGET_SECONDS <- 8
started <- proc.time()[["elapsed"]]
result <- auto_regex_infer_rules(fixture,condition_target="Options")
elapsed <- proc.time()[["elapsed"]]-started
stopifnot(elapsed <= TOTAL_BUDGET_SECONDS)
content_perf <- result$diagnostics$content_performance
stopifnot(nrow(content_perf)>0L,
  all(content_perf$ElapsedMs/1000 <= PER_LABEL_BUDGET_SECONDS))

replay <- apply_content_table(fixture,result$rules$table)$metadata
replay <- apply_condition_table(replay,result$rules$condition)$metadata
replay <- apply_ratio_table(replay,result$rules$ratio,fixture$Numerator,fixture$Denominator)$metadata
expected <- read.csv("tests/fixtures/regex_metadata_assistant/full_110_expected.csv",
  stringsAsFactors=FALSE,na.strings="NA",check.names=FALSE)
expected[] <- lapply(expected,function(x) { if(is.logical(x))x<-rep("",length(x))else if(is.character(x))x[is.na(x)]<-"";x })
stopifnot(identical(replay,expected))
cat(sprintf("110-row runtime %.3fs; slowest label %.3fs\n",elapsed,
  max(content_perf$ElapsedMs)/1000))
