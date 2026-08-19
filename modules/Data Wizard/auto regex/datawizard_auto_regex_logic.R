# Compatibility loader: keep the historical logic source path authoritative while
# evaluating extracted families in the caller's shared source environment.
source("modules/Data Wizard/auto regex/datawizard_auto_regex_diagnostics.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_replay_obligations.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_partition_recovery.R",
  local = environment())

source("modules/Data Wizard/auto regex/datawizard_auto_regex_redundancy_edges.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_redundancy_compaction.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_redundancy_rebuild.R",
  local = environment())

source("modules/Data Wizard/auto regex/datawizard_auto_regex_infer_content.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_infer_conditions.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_infer_ratios.R",
  local = environment())

source("modules/Data Wizard/auto regex/datawizard_auto_regex_reconciliation.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_pipeline.R",
  local = environment())
