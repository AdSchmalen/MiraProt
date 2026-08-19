# ============================================================================

if (!exists("datawizard_known_sample_spans", mode = "function", inherits = TRUE))
  source("modules/Data Wizard/datawizard_ratio_helpers.R", local = environment())
source("modules/Data Wizard/datawizard_condition_extraction.R", local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_contracts.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_source_identity.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_regex_primitives.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_content_search.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_condition_primitives.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_semantic_ratio_primitives.R",
  local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_rule_replay.R",
  local = environment())
