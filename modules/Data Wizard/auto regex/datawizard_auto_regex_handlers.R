# ============================================================================
# Module: Data Wizard Auto Regex handler coordinator
# Purpose: Load focused registrars and install each registrar exactly once.
# Owns: registrar dependency order and explicit handler assembly.
# Does not own: output rendering, observers, cleanup, inference, or state.
# ============================================================================

source("modules/Data Wizard/auto regex/datawizard_auto_regex_handler_support.R", local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers_source.R", local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers_redundancy.R", local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers_run.R", local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers_transfer.R", local = environment())
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers_render.R", local = environment())

auto_regex_register_handlers <- function(input, output, session, state,
                                         logger = state$logger) {
  context <- auto_regex_create_handler_context(input, output, session, state, logger)
  list2env(unclass(context), envir = environment())
  auto_regex_register_source_handlers(context)
  auto_regex_register_redundancy_handlers(context)
  auto_regex_register_workbook_handlers(context)
  auto_regex_register_run_handlers(context)
  auto_regex_register_transfer_handlers(context)
  # Rendering is registered last because it consumes state and panel helpers
  # established by the shared context and event registrars above.
  auto_regex_register_render_handlers(context)
  invisible(NULL)
}
