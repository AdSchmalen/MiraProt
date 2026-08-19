# ============================================================================
# Module: Data Wizard Auto Regex (public composition root)
# Purpose: Load the private composition layers and expose two Shiny entrypoints.
# Owns: dependency order, host-logger adapter, dependency injection, and assembly.
# Does not own: inference rules, rendering/observers, package installation,
# downloads, a Session tab, application startup/shutdown, or Auto-Assign writes.
# Public interface: modAutoRegexUI(id); modAutoRegexServer(id, metadata, data,
# revision, transfer, rule_state, provenance). The server returns session-local state for
# inspection; authoritative writes cross only the injected transfer boundary.
# Namespace: UI derives shiny::NS(id); server children use session$ns downstream.
# Dependencies: the private files sourced below, Shiny module primitives, and the
# host .miraprot_log_record recorder (with a console-only fallback, not a
# separately retained logger).
# Symbol scope: sourced into modEnv. Migrated inference compatibility names are
# intentionally unprefixed inside that private environment; new module helpers
# must use auto_regex_ (constants AUTO_REGEX_).
# ============================================================================

if (!exists("datawizard_provenance_resolve", mode = "function", inherits = TRUE))
  source("modules/Data Wizard/datawizard_provenance.R", local = modEnv)
source("modules/Data Wizard/auto regex/datawizard_auto_regex_utils.R", local = modEnv)
source("modules/Data Wizard/auto regex/datawizard_auto_regex_provenance.R", local = modEnv)
source("modules/Data Wizard/auto regex/datawizard_auto_regex_logic.R", local = modEnv)
source("modules/Data Wizard/auto regex/datawizard_auto_regex_reactive_state.R", local = modEnv)
source("modules/Data Wizard/auto regex/datawizard_auto_regex_UI.R", local = modEnv)
source("modules/Data Wizard/auto regex/datawizard_auto_regex_handlers.R", local = modEnv)

modAutoRegexUI <- function(id) {
  auto_regex_ui(shiny::NS(id))
}

modAutoRegexServer <- function(id, metadata = shiny::reactive(NULL),
                               data = shiny::reactive(NULL),
                               revision = shiny::reactive(NULL),
                               transfer = NULL, rule_state = NULL,
                               provenance = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    recorder <- get0(".miraprot_log_record", envir = globalenv(),
                     inherits = FALSE)
    fallback_warning_emitted <- FALSE
    auto_regex_log <- function(message, level = 1L, run_id = "", event_id = "") {
      level <- suppressWarnings(as.integer(level)[1L])
      if (!length(level) || is.na(level)) level <- 1L
      level <- max(0L, min(2L, level))
      message <- paste(as.character(message), collapse = " ")
      message <- trimws(gsub("[\r\n\t]+", " ", message, perl = TRUE))
      failure <- if (is.function(recorder)) tryCatch({
        recorder(level, "AUTO REGEX", message, run_id = run_id, event_id = event_id)
        NULL
      }, error = function(e) conditionMessage(e)) else "central recorder is unavailable"
      if (!is.null(failure)) {
        if (!fallback_warning_emitted) {
          warning(sprintf("AUTO REGEX central recorder failed; using console fallback: %s",
                          failure), call. = FALSE)
          fallback_warning_emitted <<- TRUE
        }
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) 0L
        )
        if (is.numeric(effective_level) && effective_level >= level) {
          cat(paste0("[ AUTO REGEX ", format(Sys.time(), "%H:%M:%S"), " ] ",
                     message), "\n")
        }
      }
      invisible(NULL)
    }
    auto_regex_log("Module initialized; central logging adapter ready.", 1L)
    state <- auto_regex_create_state(list(metadata = metadata, data = data,
                                          revision = revision, transfer = transfer,
                                          rule_state = rule_state,
                                          provenance = provenance),
                                     logger = auto_regex_log)
    auto_regex_register_handlers(input, output, session, state,
                                 logger = auto_regex_log)
    state
  })
}
