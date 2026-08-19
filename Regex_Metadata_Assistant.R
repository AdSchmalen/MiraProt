#!/usr/bin/env Rscript

# Standalone MiraProt regex metadata assistant.  This file intentionally has no
# dependency on the MiraProt module lifecycle; it only implements its persisted
# assignment-rule contract and effective PCRE/string extraction semantics.

local({

# ---- package setup ----------------------------------------------------------
required_packages <- c("shiny", "readxl", "DT", "openxlsx", "stringr")
package_repository <- "https://cloud.r-project.org"

# Logging controls are deliberately finite and centrally defined.  Keep these
# values stable so callers can safely filter and aggregate structured records.
MIRAPROT_LOG_LEVELS <- c(trace=0L, debug=1L, info=2L, warning=3L, error=4L)
MIRAPROT_LOG_COMPONENTS <- c("startup", "packages", "server", "workbook", "content", "condition", "ratio", "regex", "export")
MIRAPROT_PROCESSING_STEPS <- c("bootstrap", "install", "initialize", "upload", "inspect", "load", "map", "validate",
  "preprocess", "tokenize", "special-characters", "candidates", "abstract", "score", "anchors", "select", "conflicts",
  "boundaries", "components", "evaluate", "refine", "edit", "retest", "construct", "roundtrip", "download", "error", "reset", "shutdown")
MIRAPROT_MAX_LOG_ENTRIES <- 5000L
MIRAPROT_MAX_LOG_MESSAGE_SIZE <- 4000L
MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT <- 3L
# Short aliases make the controls convenient to use from sourced test scripts.
LOG_LEVELS <- MIRAPROT_LOG_LEVELS
LOG_COMPONENTS <- MIRAPROT_LOG_COMPONENTS
LOG_PROCESSING_STEPS <- MIRAPROT_PROCESSING_STEPS
MAX_RETAINED_LOG_ENTRIES <- MIRAPROT_MAX_LOG_ENTRIES
MAX_LOG_MESSAGE_SIZE <- MIRAPROT_MAX_LOG_MESSAGE_SIZE
REPRESENTATIVE_EXAMPLE_LIMIT <- MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT

# Stable user-facing detail ranks are deliberately independent of the severity
# values above.  Warnings and errors are milestones and therefore remain visible
# at Summary, while info/debug/trace correspond to Levels 0/1/2 respectively.
MIRAPROT_LOG_DETAIL_RANKS <- c(trace=2L, debug=1L, info=0L, warning=0L, error=0L)

.miraprot_noop_logger <- function(...) invisible(FALSE)
.miraprot_safe_value <- function(x, limit=120L) {
  value <- if (!length(x) || is.na(x[[1L]])) "<NA>" else as.character(x[[1L]])
  value <- gsub("[\r\n\t]+", " ", value)
  if (nchar(value, type="chars") > limit) paste0(substr(value, 1L, limit-1L), "…") else value
}
.miraprot_examples <- function(x) head(unique(chr(x)), MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)
.miraprot_timed <- function(logger, component, step, label, expression, level="trace") {
  started <- proc.time()[["elapsed"]]
  value <- force(expression)
  elapsed <- (proc.time()[["elapsed"]] - started) * 1000
  logger(level, component, step, sprintf("%s completed in %.1f ms.", label, elapsed),
    details=list(elapsed_ms=elapsed))
  value
}

.miraprot_effective_debug_level <- function() {
  value <- getOption("miraprot.debug.level", getOption("miraprot.debug_level",
    Sys.getenv("MIRAPROT_DEBUG_LEVEL", Sys.getenv("REGEX_METADATA_ASSISTANT_DEBUG_LEVEL", "info"))))
  key <- tolower(trimws(as.character(value)[1L]))
  if (key %in% names(MIRAPROT_LOG_LEVELS)) return(unname(MIRAPROT_LOG_LEVELS[[key]]))
  number <- suppressWarnings(as.integer(key))
  if (is.na(number)) MIRAPROT_LOG_LEVELS[["info"]] else c(2L,1L,0L)[max(0L,min(2L,number))+1L]
}

.miraprot_console_log <- function(level, component, step, message) {
  line <- sprintf("[%s] [MiraProt] [%s] [%s/%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                  toupper(level), component, step, message)
  tryCatch(message(line), error=function(e) cat(line, "\n", file=stderr()))
  invisible(NULL)
}

# This logger has no Shiny dependency and is therefore safe during package
# installation and application construction.  Calling the host recorder is
# guarded because some host implementations route records back to this helper.
.miraprot_bootstrap_logging_state <- new.env(parent=emptyenv())
.miraprot_bootstrap_logging_state$external_active <- FALSE
.miraprot_bootstrap_log <- function(level="info", component="startup", step="bootstrap", message="", event_key=NULL, details=NULL) {
  level <- tolower(as.character(level)[1L]); if (!level %in% names(MIRAPROT_LOG_LEVELS)) level <- "info"
  if (MIRAPROT_LOG_LEVELS[[level]] < .miraprot_effective_debug_level()) return(invisible(FALSE))
  component <- if (component %in% MIRAPROT_LOG_COMPONENTS) component else "startup"
  step <- if (step %in% MIRAPROT_PROCESSING_STEPS) step else "bootstrap"
  message <- substr(as.character(message)[1L], 1L, MIRAPROT_MAX_LOG_MESSAGE_SIZE)
  # The host recorder is an intentionally global compatibility hook.  Look it
  # up at call time without retaining it in this private application scope.
  recorder <- get0(".miraprot_log_record", envir=globalenv(), mode="function", inherits=FALSE)
  recorded <- FALSE
  if (is.function(recorder) && !isTRUE(.miraprot_bootstrap_logging_state$external_active)) {
    .miraprot_bootstrap_logging_state$external_active <- TRUE
    on.exit(.miraprot_bootstrap_logging_state$external_active <- FALSE, add=TRUE)
    recorded <- isTRUE(tryCatch({ recorder(level=level, component=component, step=step,
      message=message, event_key=event_key, details=details); TRUE }, error=function(e) FALSE))
  }
  if (!recorded) .miraprot_console_log(level, component, step, message)
  invisible(recorded)
}

ensure_packages <- function(packages = required_packages, repos = package_repository,
                            logger=.miraprot_bootstrap_log) {
  started <- proc.time()[["elapsed"]]
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  logger("info", "packages", "validate", sprintf("Package check found %d available and %d missing package(s).",
    length(packages)-length(missing), length(missing)), "package-check")
  for (package in packages) logger("debug", "packages", "validate", sprintf("Package '%s': %s; reason=%s.", package,
    if(package %in% missing)"install required" else "available",if(package %in% missing)"namespace not found" else "namespace resolved"))
  for (package in missing) {
    logger("info", "packages", "install", sprintf("Installing package '%s'.", package))
    installation_warnings <- character()
    installation_error <- NULL
    tryCatch(
      withCallingHandlers(
        install.packages(package, repos = repos),
        warning = function(w) {
          installation_warnings <<- c(installation_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) installation_error <<- conditionMessage(e)
    )
    if (!requireNamespace(package, quietly = TRUE)) {
      details <- c(installation_error, installation_warnings)
      details <- unique(details[nzchar(details)])
      detail_text <- if (length(details)) paste0(" Installer output: ", paste(details, collapse = " | ")) else ""
      failure <- sprintf(
        "Package '%s' could not be installed from '%s'. Check network access and library permissions, then run install.packages(\"%s\", repos = \"%s\") and restart the application.%s",
        package, repos, package, repos, detail_text
      )
      logger("error", "packages", "error", failure)
      stop(failure, call. = FALSE)
    }
  }
  unavailable <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(unavailable)) {
    package <- unavailable[[1]]
    stop(sprintf(
      "Package '%s' is unavailable after installation from '%s'. Check network access and library permissions, then run install.packages(\"%s\", repos = \"%s\") and restart the application.",
      package, repos, package, repos
    ), call. = FALSE)
  }
  logger("info", "packages", "validate", sprintf("Package checks completed: %d required package(s) available.", length(packages)),
    "package-check-complete", details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  invisible(TRUE)
}

# ---- control-flow and implementation map -----------------------------------
# Startup path
# * `launch_regex_metadata_assistant()` installs/loads the packages through `ensure_packages()`, chooses
#   browser behaviour, and passes `build_ui()` plus `server()` to Shiny.  The
#   environment-selected diagnostic path calls `run_self_tests()` instead and
#   never starts the application.
# * `build_ui()` declares, in workflow order, the workbook/sheet inputs, field
#   mapping controls, validation and inference views, editable rule tables,
#   retest diagnostics, and gated RDS/XLSX export controls.  `server()` owns all
#   state and connects those declarations to the transitions below.
#
# Interactive path (all arrows are implemented by observers/reactives in
# `server()`)
# 1. Workbook upload -> `excel_sheets()` -> sheet selection -> `read_excel()` ->
#    an unmodified preview plus the original column names.
# 2. Field mapping -> `mapped()` copies selected source columns to the contract
#    names Column and Content; the selected condition target remains named and
#    is passed separately.  Numerator, Denominator, and Options use their
#    repository names directly.
# 3. Validation -> `validate_metadata()` reports structural errors/warnings;
#    errors block the Infer observer.
# 4. Inference -> `infer_content()`, `infer_conditions()`, and `infer_ratios()`;
#    `coerce_contract()` then fixes field order/classes and the UI publishes the
#    rules, diagnostics, and warnings.
# 5. Editing -> the three `editable()` bindings update the corresponding table
#    cell in `rv$rules`; edits are deliberately not applied implicitly.
# 6. Retesting -> `validate_export()` (raw schema, then coerced values) ->
#    `apply_content_table()` -> `apply_condition_table()` ->
#    `apply_ratio_table()`; diagnostics and metrics replace inference results.
# 7. Export -> an explicit preparation action normalizes and validates once,
#    builds the Data Wizard template once, writes/reads it once, replays it, and
#    caches the verified artifact.  Dependency changes invalidate that cache;
#    downloads remain visible and only write a matching prepared artifact.
#
# Pure-function implementation map (compatibility requirement IDs refer to the
# audited contract immediately below)
# * Package/input: `ensure_packages()` makes standalone startup actionable;
#   `validate_metadata()` protects required mappings and inferability (C1).
# * Regex primitives: the `regex_*` helpers separately construct literal and
#   structural atoms, apply field-specific persistence, and validate PCRE and
#   stringr patterns; `safe_grepl()` enforces bounded,
#   case-sensitive matching (C4), and `tokens()` losslessly lexes source text
#   into typed, position-preserving spans for aligned candidate search.
# * Content search: `candidate_fragments()` bounds and ranks possible include /
#   exclude atoms; `score_pattern()` calculates complete classification evidence;
#   `refine_pattern_search()` performs bounded, audited FP/FN refinement with
#   deterministic ranking and reliability classification; `infer_anchors()`
#   adds only observation-preserving anchors; and
#   `infer_content()` selects reliable combinations, optional independent
#   redundancy, and the canonical Row Index row (C2, C4).
# * Condition helpers: `extract_condition()` is the repository-compatible
#   executor; `condition_separators()` enumerates persisted alternations;
#   `condition_contexts()` bounds boundary search; and `infer_conditions()`
#   ranks whole/start/end/between/phrase-position/pattern-detect candidates and
#   exports only exact, nonempty fits (C3, C4).  Its local `add()` constructs
#   candidates and its local prediction/accuracy calculations rank them.
# * Ratio helpers: `ratio_between()` extracts a bounded substring;
#   `ratio_normalize_boundaries()` implements coupled NumAfter/DenBefore;
#   `ratio_tokenize_exact()` reproduces repository token/gram generation;
#   `ratio_extract()` executes all three methods; `ratio_diagnostics()` compares
#   extracted pairs; and `infer_ratios()` searches method rank order and exports
#   only whole-group fits (C4, C5).  Its local `blank_rule()`, `contexts()`, and
#   `pairs()` construct correctly typed candidates and boundary pairs.
# * Application: `apply_content_table()`, `apply_condition_table()`, and
#   `apply_ratio_table()` replay persisted rule ordering and extraction while
#   retaining row-level failures (C2-C5).
# * Export: `prepare_export_inputs()` applies diagnostic-driven inclusion policy;
#   `validate_export()` checks top-level shape, exact fields/classes,
#   method-specific missing values, PCRE bounds, Row Index, and applicability;
#   `roundtrip_export()` proves serialization and loader normalization preserve
#   that contract (C1-C6).
# * Integration: `build_ui()` exposes the ordered workflow, `server()` performs
#   the state transitions above, and `run_self_tests()` exercises regex,
#   condition, inference, application, schema, complexity, and RDS round-trip
#   invariants (C1-C6).
# Maintenance completion gate: a future behavior change is complete only when
# its changed function is present in this map, its governing C1-C6 requirement
# is cited here, and a matching assertion is added to `run_self_tests()` (or the
# requirement is explicitly documented as having no executable representation).

# ---- observed Data Wizard persistence contract -----------------------------
# Contract audit (2026-07-28): this is intentionally kept beside the standalone
# implementation.  It was derived from datawizard_auto_assign.R,
# datawizard_assign_rules.R, every execution/UI/state/template helper under
# `auto assign/` and `assign rules/`, and all five shipped AutoAssign RDS files.
#
# C1 -- Container/schema compatibility
# * A rules-only template is a named list whose first three top-level components
#   are `table`, `condition`, and `ratio`; the shipped files additionally have
#   `debug_info`.  Loaders address those three frames by name and tolerate other
#   optional top-level payloads: `ui`, `UI_config`, `filter_template`,
#   `imputation_defaults`, `ratio_configurations`, `basemean_configurations`, and
#   `edit_operations`.  `assignment_rules` is a legacy component/version label,
#   not a wrapper around the three frames.  Empty optional payloads are omitted.
# C2 -- Content fields and Row Index
# * `table` columns, in exact order, are Content/Include/Exclude/Transformation;
#   all are character.  Exclude is `""` when unused.  Transformation is normally
#   `"None"`; NA_character_ is accepted for content types to which transformations
#   do not apply.  Every shipped template has the special literal Row Index rule
#   (Content and Include both `"Row Index"`, Exclude `""`, Transformation NA).
# C3 -- Condition fields
# * `condition` columns are Content/Method/Before/After/Separators/Pos: the first
#   five are character and Pos is integer.  Methods are exactly between, start,
#   end, whole, phrase_position, pattern_detect.  Before/After and Separators use
#   `""` when irrelevant; the UI still persists its integer Pos (normally 1) for
#   non-positional methods.  Separator selections are already-regex atoms
#   (`\\.`, `:`, `,`, `\\s+`, `_`, `-`, `\\(`, `\\)`) joined with bare `|`.
# C4 -- Matching and separator semantics
# * Regexes are PCRE and case-sensitive.  `|` is alternation, `^`/`$` remain
#   anchors, selected punctuation is escaped, whitespace entered through the
#   flow UI becomes `\\s+`, and `/` becomes `\\/`.  Content Include and Exclude
#   are applied independently (include AND NOT exclude); later matching rows win.
#   Condition boundaries are wrapped in `(?-i:...)`.  Position methods tokenize
#   with the persisted separator regex and use one-based positions.
#
# C5 -- Ratio fields, method-specific NA, and boundary coupling
# * `ratio` columns are Content/Method/Separators/Invert/NumBefore/NumAfter/
#   DenBefore/DenAfter/NumPos/DenPos with classes character, character,
#   character, logical, four character, integer, integer.  Regular Expressions
#   rows use NA Separators/NumPos/DenPos and Invert FALSE.  Position in String
#   rows use separator alternations and positive positions, with all four regex
#   boundaries NA.  Pattern Recognition rows use separator alternations, all
#   boundaries and positions NA, and may set Invert.  These method-specific NAs
#   (not empty strings) are present in every applicable shipped template.
# * Ratio regex extraction couples NumAfter and DenBefore when exactly one is
#   missing and skips the rule
#   when both are missing.  Pattern Recognition uses assigned Options as known
#   samples and falls back to parenthesized text.
#
# C6 -- Diagnostic envelope
# * `debug_info` is optional/diagnostic.  Shipped files contain exported_at
#   (POSIXct/POSIXt), export_options (eight scalar logical flags),
#   components_exported (including `assignment_rules`), module_versions,
#   last_import_info (optional/NULL), integer/numeric debug_level, and
#   processing_history (often an empty list).  Older optional UI fields remain
#   loader-compatible but are not manufactured by this rules-only assistant.

# ---- compatibility constants and constructors ------------------------------
CONTENT_FIELDS <- c("Content", "Include", "Exclude", "Transformation")
CONDITION_FIELDS <- c("Content", "Method", "Before", "After", "Separators", "Pos")
RATIO_FIELDS <- c("Content", "Method", "Separators", "Invert", "NumBefore", "NumAfter",
                  "DenBefore", "DenAfter", "NumPos", "DenPos")
CONDITION_METHODS <- c("between", "start", "end", "whole", "phrase_position", "pattern_detect")
RATIO_METHODS <- c("Regular Expressions", "Pattern Recognition", "Position in String")
# Keep the exact transformation-capable Data Wizard content allowlist separate
# from sample-bearing content so neither behavior is inferred from naming.
TRANSFORMATION_CONTENT_TYPES <- c(
  "Abundance Ratio",
  "Abundance Ratio p-Value",
  "Abundance Ratio Adj. p-Value",
  "Raw Abundance",
  "Normalized Abundance",
  "Batch Corrected Abundance",
  "Batch Corrected Normalized Abundance",
  "Batch Corrected Raw Abundance",
  "Imputed Raw Abundance",
  "Imputed Normalized Abundance",
  "Imputed Batch Corrected Abundance",
  "Imputed Batch Corrected Normalized Abundance",
  "Imputed Batch Corrected Raw Abundance"
)
SUPPORTED_TRANSFORMATIONS <- c("None", "log2", "log10", "-log10")

# Automatic sample-name inference is deliberately opt-in.
SAMPLE_BEARING_CONTENT_TYPES <- c(
  "Found in Sample",
  "Found in File",
  "Raw Abundance",
  "Normalized Abundance",
  "Imputed Raw Abundance",
  "Imputed Normalized Abundance",
  "Imputed Batch Corrected Abundance",
  "Batch Corrected Normalized Abundance",
  "Batch Corrected Raw Abundance",
  "Imputed Batch Corrected Normalized Abundance",
  "Imputed Batch Corrected Raw Abundance"
)

is_sample_bearing_content <- function(content) {
  trimws(as.character(content)) %in% SAMPLE_BEARING_CONTENT_TYPES
}
invalid_condition_content <- function(table) {
  if (!is.data.frame(table) || !"Content" %in% names(table)) return(character())
  unique(chr(table$Content[!is_sample_bearing_content(table$Content)]))
}
condition_content_validation_messages <- function(table) {
  invalid <- invalid_condition_content(table)
  if (!length(invalid)) return(character())
  sprintf("Condition rule Content '%s' is not sample-bearing; change its Content type or remove the rule.", invalid)
}
MAX_REGEX_LENGTH <- 2000L
MAX_REGEX_COMPLEXITY <- 100L
CONTENT_MIN_F1 <- 0.8
CONTENT_MIN_RECALL <- 0.8
CANDIDATE_FRAGMENT_SEARCH_LIMIT <- 250L
CANDIDATE_FAMILY_LIMIT <- 60L
REFINEMENT_FRONTIER_LIMIT <- 40L
REFINEMENT_CANDIDATE_LIMIT <- 400L
REFINEMENT_DEPTH_LIMIT <- 6L
CONDITION_CONTEXT_SEARCH_WIDTH <- 12L
RATIO_CONTEXT_SEARCH_WIDTH <- 16L
CONDITION_BOUNDARY_LIMIT <- 16L
CONDITION_BOUNDARY_PAIR_LIMIT <- 64L
RATIO_CONTEXT_LIMIT <- 12L
RATIO_CONTEXT_PAIR_LIMIT <- 48L
WORKBOOK_PREVIEW_ROW_LIMIT <- 100L
empty_content <- function() data.frame(Content=character(), Include=character(), Exclude=character(), Transformation=character(), stringsAsFactors=FALSE, check.names=FALSE)
empty_condition <- function() data.frame(Content=character(), Method=character(), Before=character(), After=character(), Separators=character(), Pos=integer(), check.names=FALSE)
empty_ratio <- function() data.frame(Content=character(), Method=character(), Separators=character(), Invert=logical(), NumBefore=character(), NumAfter=character(), DenBefore=character(), DenAfter=character(), NumPos=integer(), DenPos=integer(), stringsAsFactors=FALSE, check.names=FALSE)

chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

# Escape only PCRE metacharacters.  Whitespace and slash are ordinary PCRE
# literals and are intentionally not rewritten here.
regex_escape_literal <- function(x) {
  x <- chr(x)
  gsub("([\\\\.^$|()\\[\\]{}*+?])", "\\\\\\1", x, perl=TRUE)
}

regex_atom_for_token <- function(token, whitespace=c("evidence", "exact", "one_or_more", "optional"), evidence=token) {
  whitespace <- match.arg(whitespace)
  token <- chr(token)
  if (!length(token)) return(character())
  is_ws <- grepl("^[[:space:]]*$", token) & nzchar(token)
  out <- regex_escape_literal(token)
  if (any(is_ws)) {
    mode <- whitespace
    if (identical(mode, "evidence")) {
      observed <- chr(evidence)
      observed <- observed[!nzchar(observed) | grepl("^[[:space:]]+$", observed)]
      if (!length(observed)) observed <- token[is_ws]
      mode <- if (any(!nzchar(observed))) "optional" else if (length(unique(observed)) > 1L) "one_or_more" else "exact"
    }
    out[is_ws] <- switch(mode, exact=regex_escape_literal(token[is_ws]),
      one_or_more="\\s+", optional="\\s*")
  }
  out
}

regex_join_atoms <- function(atoms, operator=c("concatenate", "alternation")) {
  operator <- match.arg(operator)
  paste(chr(atoms), collapse=if (operator == "alternation") "|" else "")
}

# Data Wizard flow conversion escapes slash for content and regex-boundary
# fields, but separator selections are persisted directly.  A small scanner
# makes conversion idempotent and preserves literal backslashes.
regex_map_slashes <- function(pattern, store=TRUE) {
  map_one <- function(value) {
    if (is.na(value) || !nzchar(value)) return(value)
    chars <- strsplit(value, "", fixed=TRUE)[[1L]]; out <- character(); run <- 0L
    for (ch in chars) {
      if (ch == "\\") { run <- run + 1L; next }
      if (ch == "/") {
        if (store && run %% 2L == 0L) run <- run + 1L
        if (!store && run %% 2L == 1L) run <- run - 1L
      }
      out <- c(out, rep("\\", run), ch); run <- 0L
    }
    paste0(c(out, rep("\\", run)), collapse="")
  }
  vapply(as.character(pattern), map_one, character(1), USE.NAMES=FALSE)
}

regex_to_miraprot_storage <- function(pattern, field=c("content", "condition_boundary", "ratio_boundary", "separator")) {
  field <- match.arg(field)
  if (field == "separator") as.character(pattern) else regex_map_slashes(pattern, TRUE)
}
regex_from_miraprot_storage <- function(pattern, field=c("content", "condition_boundary", "ratio_boundary", "separator")) {
  field <- match.arg(field)
  if (field == "separator") as.character(pattern) else regex_map_slashes(pattern, FALSE)
}

regex_validation_result <- function(valid, engine, message, pattern) structure(list(
  valid=isTRUE(valid), engine=engine, message=as.character(message),
  length=if (length(pattern) && !is.na(pattern)) nchar(pattern, type="chars") else NA_integer_,
  complexity=if (length(pattern) && !is.na(pattern)) regex_complexity(pattern) else NA_integer_
), class="miraprot_regex_validation")

validate_pcre <- function(pattern) {
  pattern <- if (length(pattern)) as.character(pattern[[1L]]) else NA_character_
  issue <- NULL
  ok <- if (is.na(pattern) || !nzchar(pattern)) TRUE else isTRUE(tryCatch({
    grepl(pattern, "", perl=TRUE); TRUE
  }, warning=function(w) { issue <<- conditionMessage(w); FALSE }, error=function(e) { issue <<- conditionMessage(e); FALSE }))
  regex_validation_result(ok, "PCRE", if (ok) "" else issue, pattern)
}
validate_stringr_pattern <- function(pattern) {
  pattern <- if (length(pattern)) as.character(pattern[[1L]]) else NA_character_
  issue <- NULL
  ok <- if (is.na(pattern) || !nzchar(pattern)) TRUE else if (!requireNamespace("stringr", quietly=TRUE)) {
    issue <- "Package 'stringr' is not installed."; FALSE
  } else isTRUE(tryCatch({ stringr::str_detect("", stringr::regex(pattern)); TRUE },
    warning=function(w) { issue <<- conditionMessage(w); FALSE }, error=function(e) { issue <<- conditionMessage(e); FALSE }))
  regex_validation_result(ok, "stringr/ICU", if (ok) "" else issue, pattern)
}

# Compatibility names now expose the stricter centralized semantics.
escape_regex <- regex_escape_literal
valid_regex <- validate_pcre
safe_grepl <- function(pattern, x) {
  validation <- validate_pcre(pattern)
  if (!validation$valid || validation$length > MAX_REGEX_LENGTH) return(rep(FALSE, length(x)))
  tryCatch(grepl(paste0("(?-i:", pattern, ")"), chr(x), perl=TRUE), error=function(e) rep(FALSE,length(x)))
}

# Infer the Data Wizard value from reference rows only.  A separate details
# object supports actionable validation and bounded logging while this public
# helper retains the persisted scalar contract.
content_transformation_details <- function(df, label) {
  label <- as.character(label)[1L]
  answer <- list(value=NA_character_,source="not_applicable",message="",
    values=character(),row_ids=integer())
  if (is.na(label) || identical(label, "Row Index") ||
      !label %in% TRANSFORMATION_CONTENT_TYPES) return(answer)
  rows <- if (is.data.frame(df) && "Content" %in% names(df))
    which(!is.na(df$Content) & as.character(df$Content) == label) else integer()
  if (!"Transformation" %in% names(df)) {
    answer$value <- "None"; answer$source <- "compatible_default"
    answer$message <- "Transformation metadata is absent; using the MiraProt-compatible default 'None'."
    return(answer)
  }
  raw <- as.character(df$Transformation[rows])
  normalized <- trimws(raw)
  nonblank <- !is.na(normalized) & nzchar(normalized)
  values <- unique(normalized[nonblank]); value_rows <- rows[nonblank]
  if (!length(values)) {
    answer$value <- "None"; answer$source <- "compatible_default"
    answer$message <- "All reference transformations are blank or missing; using the MiraProt-compatible default 'None'."
    return(answer)
  }
  unsupported <- values[!values %in% SUPPORTED_TRANSFORMATIONS]
  if (length(unsupported)) {
    answer$source <- "unsupported"; answer$values <- unsupported
    answer$row_ids <- value_rows[normalized[nonblank] %in% unsupported]
    answer$message <- sprintf(
      "%s has unsupported Transformation value(s) %s at row ID(s) %s; choose exactly one of %s in the editable Content Rules table.",
      label,paste(sprintf("'%s'",unsupported),collapse=", "),paste(answer$row_ids,collapse=", "),
      paste(SUPPORTED_TRANSFORMATIONS,collapse=", "))
    return(answer)
  }
  if (length(values) > 1L) {
    answer$source <- "conflict"; answer$values <- values; answer$row_ids <- value_rows
    answer$message <- sprintf(
      "%s has conflicting Transformation values %s at row ID(s) %s; select one supported value manually in the editable Content Rules table before export.",
      label,paste(sprintf("'%s'",values),collapse=", "),paste(value_rows,collapse=", "))
    return(answer)
  }
  answer$value <- values[[1L]]; answer$source <- "metadata"; answer
}

infer_content_transformation <- function(df, label) {
  details <- content_transformation_details(df,label)
  if (details$source %in% c("unsupported", "conflict")) {
    stop(structure(list(message=details$message, call=NULL, content=as.character(label)[1L],
      values=details$values, row_ids=details$row_ids, reason=details$source),
      class=c(if(details$source=="conflict")"miraprot_transformation_ambiguity" else
          "miraprot_transformation_unsupported",
        paste0("miraprot_transformation_",details$source),
        "miraprot_transformation_validation_error","error","condition")))
  }
  details$value
}

normalize_transformation_values <- function(content, transformation) {
  content <- as.character(content)
  transformation <- as.character(transformation)
  transformation <- trimws(transformation)
  missing <- is.na(transformation) | !nzchar(transformation)
  capable <- !is.na(content) & content %in% TRANSFORMATION_CONTENT_TYPES
  transformation[missing & capable] <- "None"
  transformation[missing & !capable] <- NA_character_
  transformation
}

# ---- metadata validation ----------------------------------------------------
validate_metadata <- function(df, original_names = names(df), condition_field = "") {
  out <- data.frame(Severity=character(), Check=character(), Message=character())
  add <- function(s,c,m) out <<- rbind(out, data.frame(Severity=s,Check=c,Message=m))
  if (!is.data.frame(df) || !nrow(df)) add("Error","worksheet","Worksheet is empty.")
  if (anyDuplicated(original_names)) add("Error","column names","Duplicated worksheet column names must be resolved.")
  if (any(grepl("^\\.\\.\\.[0-9]+$", names(df)))) add("Warning","repaired names","Excel reader repaired one or more column names.")
  if (!"Column" %in% names(df)) add("Error","Column","Map a source field to the required Column field.") else {
    if (!is.character(df$Column)) add("Warning","Column type","Column was not character and will be converted without changing displayed values.")
    z <- trimws(chr(df$Column)); if (any(!nzchar(z))) add("Error","Column values","Column contains missing or whitespace-only values.")
    if (anyDuplicated(z[nzchar(z)])) add("Warning","duplicates","Column contains duplicated source values.")
  }
  empty_rows <- if (nrow(df)) apply(df,1,function(r) all(is.na(r) | !nzchar(trimws(chr(r))))) else logical()
  if (any(empty_rows)) add("Warning","empty rows",sprintf("%d entirely empty row(s) will be excluded.",sum(empty_rows)))
  if (any(duplicated(df))) add("Warning","duplicate rows","Exact duplicate metadata rows are present.")
  unsupported <- names(df)[vapply(df,function(x) is.list(x) && !is.data.frame(x),logical(1))]
  if (length(unsupported)) add("Error","types",paste("Unsupported list columns:",paste(unsupported,collapse=", ")))
  if (!"Content" %in% names(df)) add("Warning","Content","Content target unavailable; content inference will be skipped.")
  if (!"Transformation" %in% names(df)) {
    add("Warning","Transformation","Transformation metadata is absent; inference will use MiraProt-compatible defaults ('None' for transformation-capable content and NA otherwise).")
  } else if ("Content" %in% names(df)) {
    supplied <- trimws(as.character(df$Transformation))
    invalid <- !is.na(supplied) & nzchar(supplied) & !supplied %in% SUPPORTED_TRANSFORMATIONS
    if (any(invalid)) add("Error","Transformation",sprintf(
      "Unsupported Transformation value(s) %s at row ID(s) %s; accepted values are blank, NA, %s.",
      paste(sprintf("'%s'",unique(supplied[invalid])),collapse=", "),
      paste(which(invalid),collapse=", "),paste(SUPPORTED_TRANSFORMATIONS,collapse=", ")))
    labels <- unique(as.character(df$Content[!is.na(df$Content)]))
    for (label in labels[nzchar(labels)]) {
      inferred <- content_transformation_details(df,label)
      if (inferred$source == "conflict") add("Error","Transformation",inferred$message)
    }
  }
  if (!nzchar(condition_field)) add("Warning","condition target","No condition target mapped; condition inference will be skipped.")
  if (!all(c("Numerator","Denominator") %in% names(df))) add("Warning","ratio targets","Numerator/Denominator targets unavailable; ratio inference will be skipped.")
  if (!nrow(out)) add("Info","validation","No structural problems detected.")
  out
}

# ---- content candidates, scoring, redundancy, anchors ----------------------
# Lossless structural lexer.  In contrast to the old word splitter, this is a
# stable representation of the input: whitespace and punctuation are data,
# delimiters remain separate boundary spans, and Text is never normalized.
TOKEN_PUNCTUATION <- c(
  "."="period", ","="comma", ":"="colon", ";"="semicolon",
  "_"="underscore", "-"="hyphen", "/"="slash", "\\"="backslash",
  "="="equals", "+"="plus", "*"="asterisk", "&"="ampersand",
  "|"="pipe", "!"="exclamation", "?"="question", "%"="percent",
  "#"="hash", "@"="at", "$"="dollar", "^"="caret", "~"="tilde",
  "'"="apostrophe", "\""="quote", "`"="backtick", "<"="less_than",
  ">"="greater_than"
)
TOKEN_DELIMITERS <- c(
  "("="paren_open", ")"="paren_close", "["="bracket_open",
  "]"="bracket_close", "{"="brace_open", "}"="brace_close"
)

.format_special_character <- function(value) {
  names <- c("."="period", "/"="slash", "_"="underscore", "["="left bracket",
    ","="comma", ":"="colon", ";"="semicolon", "-"="hyphen", "\\"="backslash",
    "("="left parenthesis", ")"="right parenthesis", "]"="right bracket",
    "{"="left brace", "}"="right brace")
  value <- chr(value)[[1L]]
  name <- unname(names[value])
  if (length(name) && !is.na(name)) sprintf("%s (%s)", name, value)
  else sprintf("character %s", encodeString(value, quote="\""))
}

.token_base_shape <- function(text) {
  if (grepl("^[[:alpha:]]+$", text, perl=TRUE)) return("letters")
  if (grepl("^[[:digit:]]+$", text, perl=TRUE)) return("digits")
  if (grepl("^[[:alpha:]]+[[:digit:]]+$", text, perl=TRUE)) return("prefix_numeric_suffix")
  if (grepl("^[[:digit:]]+[[:alpha:]]+$", text, perl=TRUE)) return("numeric_prefix_letters")
  "mixed_identifier"
}

tokens <- function(x) {
  source <- chr(x)
  columns <- list(Source=integer(), Span=integer(), Start=integer(), End=integer(),
                  Text=character(), Type=character(), Shape=character(),
                  BaseShape=character(), Normalized=character(),
                  Parenthesized=logical(), ReplicateLike=logical(),
                  ConditionLike=logical())
  empty <- as.data.frame(columns, stringsAsFactors=FALSE, check.names=FALSE)
  lex_one <- function(value, source_index) {
    chars <- strsplit(value, "", fixed=TRUE)[[1L]]
    if (!length(chars)) return(empty)
    rows <- vector("list", length(chars)); row_count <- 0L; i <- 1L
    while (i <= length(chars)) {
      char <- chars[[i]]
      is_space <- grepl("^[[:space:]]$", char, perl=TRUE)
      is_word <- grepl("^[[:alnum:]]$", char, perl=TRUE)
      if (is_space || is_word) {
        j <- i
        while (j < length(chars) && grepl(if (is_space) "^[[:space:]]$" else "^[[:alnum:]]$",
                                          chars[[j+1L]], perl=TRUE)) j <- j+1L
        text <- paste0(chars[i:j], collapse="")
        type <- if (is_space) "whitespace" else "identifier"
        base_shape <- if (is_space) "whitespace" else .token_base_shape(text)
      } else {
        j <- i; text <- char
        if (char %in% names(TOKEN_DELIMITERS)) {
          type <- unname(TOKEN_DELIMITERS[[char]]); base_shape <- "delimiter"
        } else if (char %in% names(TOKEN_PUNCTUATION)) {
          type <- unname(TOKEN_PUNCTUATION[[char]]); base_shape <- "punctuation"
        } else {
          type <- "unknown"; base_shape <- "unknown"
        }
      }
      row_count <- row_count+1L
      rows[[row_count]] <- data.frame(Source=source_index, Span=row_count,
        Start=i, End=j, Text=text, Type=type, Shape=base_shape,
        BaseShape=base_shape, Normalized=tolower(text), Parenthesized=FALSE,
        ReplicateLike=FALSE, ConditionLike=FALSE, stringsAsFactors=FALSE,
        check.names=FALSE)
      i <- j+1L
    }
    do.call(rbind, rows[seq_len(row_count)])
  }
  result <- do.call(rbind, lapply(seq_along(source), function(i) lex_one(source[[i]], i)))
  if (is.null(result) || !nrow(result)) return(empty)
  rownames(result) <- NULL

  # These contextual shapes annotate one span without consuming its delimiter
  # neighbours.  Thus every character continues to have exactly one owner.
  by_source <- split(seq_len(nrow(result)), result$Source)
  for (indices in by_source) {
    if (length(indices) >= 3L) for (position in 2:(length(indices)-1L)) {
      row <- indices[[position]]
      if (result$Type[row] == "identifier" &&
          result$Type[indices[[position-1L]]] == "paren_open" &&
          result$Type[indices[[position+1L]]] == "paren_close") {
        result$Parenthesized[row] <- TRUE
        result$Shape[row] <- "parenthesized_value"
      }
    }
    identifier_rows <- indices[result$Type[indices] == "identifier"]
    replicate_rows <- identifier_rows[grepl("^(?:rep(?:licate)?|r)[[:digit:]]+$",
      result$Normalized[identifier_rows], perl=TRUE)]
    result$ReplicateLike[replicate_rows] <- TRUE
    result$Shape[replicate_rows] <- "replicate_like"
  }

  # Learn condition candidates from variation in structurally aligned training
  # strings.  There is deliberately no vocabulary of biological conditions:
  # only a varying value at an otherwise matching span position is evidence.
  if (length(by_source) > 1L && length(unique(lengths(by_source))) == 1L) {
    width <- lengths(by_source)[[1L]]
    for (position in seq_len(width)) {
      aligned <- vapply(by_source, function(indices) indices[[position]], integer(1))
      same_type <- length(unique(result$Type[aligned])) == 1L
      variable <- length(unique(result$Normalized[aligned])) > 1L
      eligible <- all(result$Type[aligned] == "identifier")
      if (same_type && variable && eligible) {
        result$ConditionLike[aligned] <- TRUE
        plain <- aligned[!result$Parenthesized[aligned] & !result$ReplicateLike[aligned]]
        result$Shape[plain] <- "condition_like"
      }
    }
  }
  result
}
common_values <- function(xs) Reduce(intersect, lapply(xs, unique))

.candidate_token_rows <- function(values) {
  lapply(seq_along(values), function(i) {
    z <- tokens(values[[i]]); z[z$Source == 1L,,drop=FALSE]
  })
}

.shape_atom <- function(shape) switch(shape,
  letters="[[:alpha:]]+", digits="[[:digit:]]+",
  prefix_numeric_suffix="[[:alpha:]]+[[:digit:]]+",
  numeric_prefix_letters="[[:digit:]]+[[:alpha:]]+",
  replicate_like="[[:alpha:]]+[[:digit:]]+",
  mixed_identifier="[[:alnum:]]+", condition_like="[[:alnum:]]+", NULL)

# A literal is discriminative when its observed class support is high, its
# opposing support is materially lower, and erasing the term buys no recall.
# The final comparison is contextual, so a broad word atom is not allowed to
# replace a class-defining term merely because it has the same token type.
.protect_literal <- function(literal, generalized, pos, neg) {
  literal <- regex_to_miraprot_storage(regex_atom_for_token(literal), "content")
  lp <- safe_grepl(literal,pos); gp <- safe_grepl(generalized,pos)
  ln <- safe_grepl(literal,neg); gn <- safe_grepl(generalized,neg)
  mean(lp) >= .75 && (mean(lp) - if(length(ln)) mean(ln) else 0) >= .25 &&
    sum(gp) <= sum(lp) && sum(gn) > sum(ln)
}

.candidate_family_builders <- function(values, opposing=character()) {
  values <- unique(chr(values)); values <- values[nzchar(values)]
  if (!length(values)) return(list(structural=character(), shape=character(),
                                   partial=character(), concrete=character()))
  seqs <- .candidate_token_rows(values)
  types <- lapply(seqs, `[[`, "Type")
  widths <- lengths(types); aligned <- length(unique(widths)) == 1L &&
    all(vapply(types[-1L], identical, logical(1), types[[1L]]))
  structural <- shape <- partial <- concrete <- character()
  literal_atom <- function(x) regex_to_miraprot_storage(regex_atom_for_token(x), "content")
  atom_for_column <- function(column, shapes=FALSE) {
    text <- vapply(seqs, function(z) z$Text[[column]], character(1))
    type <- vapply(seqs, function(z) z$Type[[column]], character(1))
    base <- vapply(seqs, function(z) z$BaseShape[[column]], character(1))
    if (length(unique(text)) == 1L && !(shapes && all(type == "identifier")))
      return(literal_atom(text[[1L]]))
    if (all(type == "whitespace")) return(regex_atom_for_token(" ", evidence=text))
    # Stable punctuation is structural, never a wildcard.
    if (all(type != "identifier")) return(NULL)
    same_shape <- length(unique(base)) == 1L &&
      (length(unique(tolower(text))) > 1L || (shapes && length(text) == 1L))
    atom <- if (same_shape) .shape_atom(base[[1L]]) else NULL
    if (is.null(atom)) return(NULL)
    protected <- any(vapply(unique(text), .protect_literal, logical(1),
                            generalized=atom, pos=values, neg=opposing))
    if (protected && !shapes) return(paste0("(?:",paste(literal_atom(unique(text)),collapse="|"),")"))
    atom
  }
  if (aligned) {
    atoms <- lapply(seq_len(widths[[1L]]), atom_for_column)
    if (all(lengths(atoms) == 1L)) structural <- paste0(unlist(atoms),collapse="")
    shape_atoms <- lapply(seq_len(widths[[1L]]), atom_for_column, shapes=TRUE)
    if (all(lengths(shape_atoms) == 1L)) shape <- paste0(unlist(shape_atoms),collapse="")
    invariant <- which(vapply(seq_len(widths[[1L]]), function(j)
      length(unique(vapply(seqs,function(z)z$Text[[j]],character(1)))) == 1L, logical(1)))
    informative <- invariant[vapply(invariant,function(j) seqs[[1L]]$Type[[j]] == "identifier" &&
      nchar(seqs[[1L]]$Text[[j]]) >= 2L,logical(1))]
    partial <- literal_atom(vapply(informative,function(j)seqs[[1L]]$Text[[j]],character(1)))
  } else {
    # Relative-position alignment: preserve the longest common typed/literal
    # prefix and suffix and treat only the intervening span as optional.
    minw <- min(widths)
    compatible <- function(j, from_end=FALSE) {
      rows <- mapply(function(z,w) if(from_end) w-j+1L else j, seqs, widths)
      ts <- mapply(function(z,k) z$Type[[k]],seqs,rows,USE.NAMES=FALSE)
      tx <- mapply(function(z,k) z$Text[[k]],seqs,rows,USE.NAMES=FALSE)
      bs <- mapply(function(z,k) z$BaseShape[[k]],seqs,rows,USE.NAMES=FALSE)
      length(unique(ts)) == 1L && (length(unique(tx)) == 1L ||
        (from_end && ts[[1L]] == "identifier" && length(unique(bs)) == 1L) ||
        ts[[1L]] == "whitespace")
    }
    pre <- 0L; while(pre < minw && compatible(pre+1L)) pre <- pre+1L
    suf <- 0L; while(suf < minw-pre && compatible(suf+1L,TRUE)) suf <- suf+1L
    edge_atom <- function(j,end=FALSE) {
      text <- mapply(function(z,w) z$Text[[if(end) w-j+1L else j]],seqs,widths,USE.NAMES=FALSE)
      if(length(unique(text)) == 1L) literal_atom(text[[1L]]) else {
        bases <- mapply(function(z,w) z$BaseShape[[if(end) w-j+1L else j]],seqs,widths,USE.NAMES=FALSE)
        if(length(unique(bases)) == 1L) .shape_atom(bases[[1L]]) else NULL
      }
    }
    pa <- if(pre) vapply(seq_len(pre),edge_atom,character(1)) else character()
    sa <- if(suf) vapply(rev(seq_len(suf)),edge_atom,character(1),end=TRUE) else character()
    middle <- mapply(function(z,w) {
      first <- pre+1L; last <- w-suf
      if(first > last) return("")
      atoms <- vapply(first:last,function(k) {
        if(z$Type[[k]] == "identifier") .shape_atom(z$BaseShape[[k]])
        else if(z$Type[[k]] == "whitespace") regex_atom_for_token(" ",evidence=z$Text[[k]])
        else literal_atom(z$Text[[k]])
      },character(1))
      paste0(atoms,collapse="")
    },seqs,widths,USE.NAMES=FALSE)
    middle <- unique(middle)
    has_empty <- "" %in% middle; middle <- middle[nzchar(middle)]
    middle_atom <- if(!length(middle)) "" else paste0("(?:",paste(middle,collapse="|"),")",
      if(has_empty) "?" else "")
    if(length(pa)+length(sa) && all(nzchar(c(pa,sa))))
      structural <- paste0(c(pa,middle_atom,sa),collapse="")
  }
  concrete <- c(literal_atom(values), unlist(lapply(seqs,function(z) {
    ids <- z$Text[z$Type == "identifier" & nchar(z$Text) >= 2L]
    c(literal_atom(ids), if(length(ids)>1L) paste(literal_atom(ids[-length(ids)]),
      literal_atom(ids[-1L]),sep="[^[:alnum:]]+") else character())
  }),use.names=FALSE))
  list(structural=structural,shape=shape,partial=partial,concrete=concrete)
}

candidate_fragments <- function(pos, neg=character(), limit=CANDIDATE_FRAGMENT_SEARCH_LIMIT) {
  # Families are intentionally ordered from most generalized to most concrete.
  # Negative families are included so the same bounded list can supply safe
  # exclusions.  Semantic keys prevent equivalent runtime patterns generated
  # by two builders from occupying the search budget twice.
  pf <- .candidate_family_builders(pos,neg)
  nf <- .candidate_family_builders(neg,pos)
  family_names <- c("structural","shape","partial","concrete")
  records <- do.call(rbind,lapply(family_names,function(family) {
    patterns <- c(pf[[family]],nf[[family]])
    patterns <- unique(patterns[nzchar(patterns)])
    if(!length(patterns)) return(NULL)
    data.frame(Pattern=patterns,Family=family,Constraint=family,stringsAsFactors=FALSE)
  }))
  if(is.null(records) || !nrow(records)) return(character())
  records$Runtime <- regex_from_miraprot_storage(records$Pattern,"content")
  records$Key <- paste(records$Runtime,records$Constraint,sep="\034")
  records <- records[!duplicated(records$Key),,drop=FALSE]
  records <- do.call(rbind,lapply(family_names,function(f) head(records[records$Family==f,,drop=FALSE],
    as.integer(CANDIDATE_FAMILY_LIMIT))))
  selected<-head(records,as.integer(limit))
  result<-selected$Pattern
  attr(result,"candidate_families")<-selected$Family
  result
}
.regex_constraint_count <- function(pattern, exclude="") {
  # Position is a transformation of a constraint, not another constraint.
  z <- gsub("(?:^\\^|\\$$)","",paste(chr(c(pattern,exclude)),collapse=""),perl=TRUE)
  if(!nzchar(z)) return(0L)
  as.integer(1L+nzchar(exclude)+lengths(regmatches(z,gregexpr("\\(\\?=|\\(\\?!|\\||\\^|\\$|\\{|\\+|\\*|\\?",z,perl=TRUE))))
}

# Score the effective include/exclude rule against every labelled row.  The
# legacy long names are retained because the diagnostics UI consumes them.
score_pattern <- function(pattern, x, truth, exclude="", constraint_count=NULL) {
  x<-chr(x); truth<-as.logical(truth); truth[is.na(truth)]<-FALSE
  if(length(x)!=length(truth)) stop("x and truth must have identical lengths.",call.=FALSE)
  include_validation<-validate_pcre(regex_from_miraprot_storage(pattern,"content"))
  exclude_validation<-validate_pcre(regex_from_miraprot_storage(exclude,"content"))
  if(!include_validation$valid || !exclude_validation$valid)
    stop(paste("Invalid pattern:",paste(c(include_validation$message,exclude_validation$message)[nzchar(c(include_validation$message,exclude_validation$message))],collapse="; ")),call.=FALSE)
  hit<-safe_grepl(pattern,x); if(nzchar(exclude)) hit<-hit&!safe_grepl(exclude,x)
  tp<-sum(hit&truth); fp<-sum(hit&!truth); fn<-sum(!hit&truth); tn<-sum(!hit&!truth)
  precision<-if(tp+fp)tp/(tp+fp)else 0; recall<-if(tp+fn)tp/(tp+fn)else 0
  specificity<-if(tn+fp)tn/(tn+fp)else 0
  f1<-if(precision+recall)2*precision*recall/(precision+recall)else 0
  balanced<-(recall+specificity)/2
  # Deterministic leave-one-out recall.  Regex rules are not fitted here, so it
  # measures observation stability without introducing a random fold split.
  cv_recall<-if(any(truth)) mean(hit[truth]) else 0
  constraints<-if(is.null(constraint_count)).regex_constraint_count(pattern,exclude)else as.integer(constraint_count)
  complexity<-regex_complexity(regex_from_miraprot_storage(pattern,"content"))+
    if(nzchar(exclude))regex_complexity(regex_from_miraprot_storage(exclude,"content"))else 0L
  data.frame(TP=tp,TN=tn,FP=fp,FN=fn,Precision=precision,Recall=recall,F1=f1,
    Specificity=specificity,BalancedAccuracy=balanced,Coverage=if(length(hit))mean(hit)else 0,
    RegexLength=nchar(pattern)+nchar(exclude),Complexity=complexity,ConstraintCount=constraints,
    CrossValidationRecall=cv_recall,FalsePositives=fp,FalseNegatives=fn,stringsAsFactors=FALSE)
}

.refinement_tier <- function(m, conflicting=FALSE) {
  if(conflicting) return("conflicting")
  if(m$F1>=CONTENT_MIN_F1 && m$Recall>=CONTENT_MIN_RECALL && m$FP==0L) "reliable"
  else if(m$F1>=CONTENT_MIN_F1 && m$Recall>=CONTENT_MIN_RECALL) "provisional"
  else "unresolved"
}

.refinement_order <- function(candidates) {
  tier<-match(candidates$Tier,c("reliable","provisional","unresolved","conflicting"))
  order(tier,candidates$FN,candidates$FP,-candidates$CrossValidationRecall,
    -candidates$Specificity,candidates$ConstraintCount,candidates$Complexity,
    candidates$RegexLength,-candidates$Generalization,candidates$Include,
    candidates$Exclude,candidates$CandidateID,method="radix")
}

.refinement_mutations <- function(candidate,x,truth) {
  hit<-safe_grepl(candidate$Include,x); if(nzchar(candidate$Exclude))hit<-hit&!safe_grepl(candidate$Exclude,x)
  fp<-x[hit&!truth]; fn<-x[!hit&truth]; pos<-x[truth]; neg<-x[!truth]
  out<-list(); add<-function(action,include=candidate$Include,exclude=candidate$Exclude,reason) {
    out[[length(out)+1L]]<<-list(action=action,include=include,exclude=exclude,reason=reason)
  }
  if(length(fp)) {
    stable<-candidate_fragments(pos,neg,60L)
    context<-stable[vapply(stable,function(p)all(safe_grepl(p,pos))&&any(!safe_grepl(p,fp)),logical(1))]
    if(length(context)) add("add_stable_structural_context",paste0("(?=.*(?:",context[[1L]],"))",candidate$Include),reason="stable context occurs in every positive and excludes an observed false positive")
    literals<-unique(tokens(pos)$Text); literals<-literals[nchar(literals)>=2L]
    informative<-literals[vapply(literals,function(z)all(safe_grepl(regex_escape_literal(z),pos))&&!all(safe_grepl(regex_escape_literal(z),fp)),logical(1))]
    if(length(informative)) add("add_protected_literal",paste0("(?=.*(?:",regex_escape_literal(sort(informative)[[1L]]),"))",candidate$Include),reason="literal is invariant in positives and informative against false positives")
    harmless<-candidate_fragments(fp,pos,60L); harmless<-harmless[vapply(harmless,function(p)!any(safe_grepl(p,pos))&&any(safe_grepl(p,fp)),logical(1))]
    if(length(harmless)) add("add_harmless_exclusion",exclude=harmless[[1L]],reason="exclusion matches a false positive and no positive")
    anchored<-infer_anchors(candidate$Include,pos,fp)
    if(!identical(anchored,candidate$Include))add("apply_justified_anchor",anchored,reason="anchor preserves all positive matches and removes opposing matches")
    classes<-c("[[:alnum:]]+"="[[:alpha:]]+",".*"="[^[:space:]]+",".+"="[^[:space:]]+")
    for(broad in names(classes))if(grepl(broad,candidate$Include,fixed=TRUE)){add("specialize_broadest_class",sub(broad,classes[[broad]],candidate$Include,fixed=TRUE),reason="observed positive span supports the narrower class");break}
    if(grepl("|",candidate$Include,fixed=TRUE)) {
      parts<-strsplit(candidate$Include,"|",fixed=TRUE)[[1L]]; supported<-parts[vapply(parts,function(p)any(safe_grepl(p,pos)),logical(1))]
      if(length(supported)&&length(supported)<length(parts))add("narrow_unsupported_alternatives",paste(supported,collapse="|"),reason="removed alternatives match no positive observation")
    }
  }
  if(length(fn)) {
    literal_tokens<-unique(tokens(candidate$Include)$Text); literal_tokens<-literal_tokens[grepl("^[[:alnum:]]{2,}$",literal_tokens)]
    unsupported<-literal_tokens[!vapply(literal_tokens,function(z)all(grepl(z,pos,fixed=TRUE)),logical(1))]
    if(length(unsupported))add("remove_unsupported_literal",gsub(regex_escape_literal(unsupported[[1L]]),"",candidate$Include,fixed=TRUE),reason="literal is absent from at least one positive")
    generalized<-gsub("[[:digit:]]+","[[:digit:]]+",candidate$Include,perl=TRUE)
    if(!identical(generalized,candidate$Include))add("generalize_numeric_or_identifier_span",generalized,reason="positive false negatives demonstrate a variable numeric span")
    relaxed<-sub("\\\\s\\+","\\\\s*",candidate$Include)
    if(!identical(relaxed,candidate$Include)&&any(grepl("[[:space:]]",pos)))add("relax_supported_whitespace",relaxed,reason="observed positives support optional whitespace")
    separators<-unique(tokens(fn)$Text); separators<-sort(separators[grepl("^[[:punct:]]$",separators)])
    if(length(separators))add("add_observed_separator_alternative",paste0("(?:",candidate$Include,"|",regex_escape_literal(separators[[1L]]),")"),reason="separator alternative is directly observed in a missed positive")
    unanchored<-sub("^\\^","",sub("\\$$","",candidate$Include))
    if(!identical(unanchored,candidate$Include))add("remove_unsupported_anchor",unanchored,reason="anchor excludes an observed positive")
    run<-sub("\\{[0-9]+\\}","+",candidate$Include,perl=TRUE)
    if(!identical(run,candidate$Include))add("generalize_supported_run_length",run,reason="positive variation does not support a fixed run length")
  }
  out
}

# Bounded deterministic best-first refinement.  Every disposition is retained
# in `audit`, including invalid/over-limit proposals and an explicit abstention.
refine_pattern_search <- function(pattern,x,truth,exclude="",max_depth=REFINEMENT_DEPTH_LIMIT,
  max_candidates=REFINEMENT_CANDIDATE_LIMIT,frontier_limit=REFINEMENT_FRONTIER_LIMIT,
  max_length=MAX_REGEX_LENGTH,max_complexity=MAX_REGEX_COMPLEXITY,quality_f1=CONTENT_MIN_F1) {
  x<-chr(x); truth<-as.logical(truth); conflict<-any(x[truth]%in%x[!truth])
  frontier<-data.frame(Include=pattern,Exclude=exclude,Depth=0L,ParentID=NA_integer_,Action="initial",Reason="initial candidate",Generalization=0L,stringsAsFactors=FALSE)
  accepted<-data.frame(); audit<-data.frame(CandidateID=integer(),ParentID=integer(),Depth=integer(),Action=character(),Disposition=character(),Reason=character(),Include=character(),Exclude=character(),stringsAsFactors=FALSE)
  seen<-character(); next_id<-1L; stop_reason<-"frontier_exhausted"
  while(nrow(frontier)&&next_id<=max_candidates) {
    row<-frontier[1L,,drop=FALSE]; frontier<-frontier[-1L,,drop=FALSE]; id<-next_id; next_id<-next_id+1L
    key<-paste(row$Include,row$Exclude,sep="\034")
    disposition<-"accepted"; reason<-row$Reason
    validations<-c(validate_pcre(regex_from_miraprot_storage(row$Include,"content"))$valid,validate_pcre(regex_from_miraprot_storage(row$Exclude,"content"))$valid)
    length_now<-nchar(row$Include)+nchar(row$Exclude)
    complexity_now<-regex_complexity(regex_from_miraprot_storage(row$Include,"content"))+if(nzchar(row$Exclude))regex_complexity(regex_from_miraprot_storage(row$Exclude,"content"))else 0L
    if(key%in%seen){disposition<-"rejected";reason<-"duplicate stored pattern"}
    else if(!all(validations)){disposition<-"rejected";reason<-"invalid PCRE pattern"}
    else if(length_now>max_length){disposition<-"rejected";reason<-"length limit exceeded"}
    else if(complexity_now>max_complexity){disposition<-"rejected";reason<-"complexity limit exceeded"}
    audit<-rbind(audit,data.frame(CandidateID=id,ParentID=as.integer(row$ParentID),Depth=row$Depth,Action=row$Action,Disposition=disposition,Reason=reason,Include=row$Include,Exclude=row$Exclude,stringsAsFactors=FALSE))
    if(disposition=="rejected")next
    seen<-c(seen,key); m<-score_pattern(row$Include,x,truth,row$Exclude)
    tier<-.refinement_tier(m,conflict)
    accepted<-rbind(accepted,cbind(data.frame(CandidateID=id,ParentID=as.integer(row$ParentID),Depth=row$Depth,Action=row$Action,Reason=reason,Include=row$Include,Exclude=row$Exclude,Tier=tier,Generalization=row$Generalization,stringsAsFactors=FALSE),m))
    if(tier=="reliable"&&m$F1>=quality_f1){stop_reason<-"quality_limit_reached";break}
    if(row$Depth>=max_depth){stop_reason<-"depth_limit_reached";next}
    children<-.refinement_mutations(row,x,truth)
    if(length(children))for(child in children)frontier<-rbind(frontier,data.frame(Include=child$include,Exclude=child$exclude,Depth=row$Depth+1L,ParentID=id,Action=child$action,Reason=child$reason,Generalization=row$Generalization+as.integer(grepl("remove|generalize|relax",child$action)),stringsAsFactors=FALSE))
    if(nrow(frontier)>frontier_limit){
      dropped<-frontier[-seq_len(frontier_limit),,drop=FALSE]
      audit<-rbind(audit,data.frame(CandidateID=NA_integer_,ParentID=dropped$ParentID,
        Depth=dropped$Depth,Action=dropped$Action,Disposition="rejected",
        Reason="frontier limit exceeded",Include=dropped$Include,Exclude=dropped$Exclude,
        stringsAsFactors=FALSE))
      frontier<-frontier[seq_len(frontier_limit),,drop=FALSE];stop_reason<-"frontier_limit_applied"
    }
  }
  if(next_id>max_candidates&&nrow(frontier))stop_reason<-"candidate_limit_reached"
  if(!nrow(accepted))return(list(status=if(conflict)"conflicting"else"unresolved",best=NULL,candidates=accepted,audit=audit,stop_reason=stop_reason,abstention=list(code="no_valid_candidate",reason="all candidates were rejected before scoring")))
  accepted<-accepted[.refinement_order(accepted),,drop=FALSE];best<-accepted[1L,,drop=FALSE]
  abstention<-if(best$Tier%in%c("unresolved","conflicting"))list(code=paste0(best$Tier,"_evidence"),reason=if(conflict)"identical source text has conflicting labels"else"quality thresholds were not met")else NULL
  list(status=best$Tier,best=best,candidates=accepted,audit=audit,stop_reason=stop_reason,abstention=abstention)
}
infer_anchors <- function(fragment, measured, opposing=character()) {
  # Candidate records are accepted directly; character input remains supported
  # for callers outside the assistant.  Start and end anchoring are deliberately
  # independent transformations of the discriminating pattern.
  object <- is.list(fragment) && !is.data.frame(fragment)
  pattern <- if(object) { z<-fragment$pattern;if(is.null(z))z<-fragment$Pattern;if(is.null(z))z<-fragment$Include;chr(z)[1L] } else chr(fragment)[1L]
  positives <- chr(measured); negatives <- chr(opposing)
  quality <- function(p) {
    hitp<-safe_grepl(p,positives); hitn<-safe_grepl(p,negatives)
    tp<-sum(hitp);fp<-sum(hitn);fn<-length(hitp)-tp;tn<-length(hitn)-fp
    recall<-if(length(hitp))tp/length(hitp)else 0; specificity<-if(length(hitn))tn/length(hitn)else 1
    precision<-if(tp+fp)tp/(tp+fp)else 0
    c(recall=recall,balanced=(recall+specificity)/2,f1=if(precision+recall)2*precision*recall/(precision+recall)else 0,fp=fp)
  }
  base_coverage <- safe_grepl(pattern,positives); current<-pattern; history<-data.frame()
  for(side in c("start","end")) {
    proposal<-if(side=="start")paste0("^",current)else paste0(current,"$")
    before<-quality(current);after<-quality(proposal)
    accepted<-identical(safe_grepl(proposal,positives),base_coverage) &&
      after[["recall"]]>=before[["recall"]] && after[["balanced"]]>=before[["balanced"]] &&
      after[["f1"]]>=before[["f1"]]
    history<-rbind(history,data.frame(Transformation=paste0("anchor_",side),Before=current,
      After=proposal,Accepted=accepted,DeltaFalsePositives=after[["fp"]]-before[["fp"]],
      Reason=if(accepted)"positive coverage preserved and classification quality preserved or improved" else "positive coverage or classification quality would decrease",stringsAsFactors=FALSE))
    if(accepted)current<-proposal
  }
  if(!object)return(current)
  fragment$pattern<-current;fragment$anchor_history<-history;fragment
}
infer_content <- function(df, redundancy=0L, logger=function(...) invisible(NULL)) {
  started<-proc.time()[["elapsed"]]
  logger("debug","content","infer",sprintf("Inferring content rules from %d rows.",nrow(df)),
    details=list(examples=head(chr(df$Column),MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)))
  x<-chr(df$Column);y<-chr(df$Content);row_id<-seq_along(x)
  labels<-unique(y[nzchar(trimws(y))]) # table order is evidence; never sort it
  rules<-empty_content();metrics<-data.frame();statuses<-data.frame();refinement_history<-list()
  redundancy_history<-data.frame();unresolved<-data.frame();representatives<-data.frame();notes<-character()
  add_unresolved<-function(label,code,reason) {
    unresolved<<-rbind(unresolved,data.frame(Content=label,Code=code,Reason=reason,stringsAsFactors=FALSE))
    if(!label%in%statuses$Content)statuses<<-rbind(statuses,data.frame(Content=label,Status="unresolved",
      Reliable=FALSE,PositiveExamples=sum(y==label),NegativeExamples=sum(y!=label),stringsAsFactors=FALSE))
    notes<<-c(notes,paste0(label,": ",reason))
  }
  for(label in labels) {
    truth<-y==label;pos<-x[truth];neg<-x[!truth]
    transformation_details<-content_transformation_details(df,label)
    transformation<-if(transformation_details$source %in% c("conflict","unsupported"))
      NA_character_ else infer_content_transformation(df,label)
    transformation_source<-transformation_details$source
    transformation_message<-transformation_details$message
    if(transformation_source %in% c("conflict","unsupported")) {
      notes<-c(notes,transformation_message)
      unresolved<-rbind(unresolved,data.frame(Content=label,
        Code=paste0("transformation_",transformation_source),Reason=transformation_message,stringsAsFactors=FALSE))
      logger("warning","content","conflicts",transformation_message,
        details=list(content=label,values=transformation_details$values,row_ids=transformation_details$row_ids))
    } else logger("debug","content","select",sprintf("%s: Transformation=%s; source=%s.",label,
      if(is.na(transformation))"NA" else transformation,transformation_source))
    logger("trace","content","tokenize",sprintf("%s: representative tokenizations prepared for row IDs %s.",
      label,paste(head(row_id[truth],3L),collapse=",")),details=list(examples=.miraprot_examples(pos)))
    special<-unique(tokens(pos)$Text);special<-special[grepl("^[^[:alnum:][:space:]]$",special)]
    formatted_special <- vapply(head(special,3L), .format_special_character, character(1))
    logger("trace","content","special-characters",sprintf("%s: detected %d distinct special character(s); values: %s",
      label,length(special),paste(formatted_special,collapse=", ")))
    # Row Index is a Data Wizard protocol rule rather than an inferred content
    # class.  Preserve it once, canonically, regardless of its sample count.
    if(identical(label,"Row Index")) {
      statuses<-rbind(statuses,data.frame(Content=label,Status="reliable",Reliable=TRUE,
        PositiveExamples=length(pos),NegativeExamples=length(neg),stringsAsFactors=FALSE))
      next
    }
    if(any(pos%in%neg)){add_unresolved(label,"conflicting_labels","identical source text occurs under another label");next}
    raw<-candidate_fragments(pos,neg)
    # Normalize the new record-shaped candidate object as well as legacy vectors.
    records<-if(is.data.frame(raw))raw else if(is.list(raw)&&!is.null(raw$candidates))raw$candidates else {
      families<-attr(raw,"candidate_families",exact=TRUE)
      if(is.null(families)||length(families)!=length(raw))families<-rep("legacy",length(raw))
      data.frame(Pattern=chr(raw),Family=chr(families),stringsAsFactors=FALSE)
    }
    logger("trace","content","candidates",sprintf("%s: generated %d bounded candidates; trace: %s.",label,nrow(records),
      paste(vapply(head(records$Pattern,3L),.miraprot_safe_value,character(1),limit=80L),collapse=" | ")))
    logger("trace","content","abstract",sprintf("%s: candidate families abstracted %d positive and %d negative examples without retaining source frames.",label,length(pos),length(neg)))
    if(!nrow(records)){add_unresolved(label,"no_candidate","no stable discriminating candidate was generated");next}
    anchored<-lapply(seq_len(nrow(records)),function(i)infer_anchors(list(pattern=records$Pattern[i],candidate=records[i,,drop=FALSE]),pos,neg))
    anchor_decisions<-do.call(rbind,lapply(anchored,`[[`,"anchor_history"))
    logger("trace","content","anchors",sprintf("%s: anchor inference accepted %d of %d bounded transformations.",label,sum(anchor_decisions$Accepted),nrow(anchor_decisions)))
    inc<-unique(vapply(anchored,`[[`,character(1),"pattern"));inc<-inc[vapply(inc,function(p)all(safe_grepl(p,pos)),logical(1))]
    exc_raw<-records$Pattern[vapply(records$Pattern,function(p)any(safe_grepl(p,neg))&&!any(safe_grepl(p,pos)),logical(1))]
    exc<-unique(vapply(exc_raw,infer_anchors,character(1),measured=neg,opposing=pos))
    if(!length(inc)){add_unresolved(label,"no_positive_coverage","no candidate matches every applicable positive");next}
    sets<-c(lapply(inc,function(i)list(include=i,exclude="")),
      unlist(lapply(inc,function(i)lapply(exc,function(e)list(include=i,exclude=e))),recursive=FALSE))
    family_for<-function(pattern) {
      matched<-which(vapply(anchored,function(z)identical(z$pattern,pattern),logical(1)))
      if(!length(matched))return("concrete")
      chr(anchored[[matched[[1L]]]]$candidate$Family)[[1L]]
    }
    evaluated<-do.call(rbind,lapply(sets,function(z)cbind(data.frame(Include=z$include,Exclude=z$exclude,
      Family=family_for(z$include),BaseConstraints=1L+nzchar(z$exclude),stringsAsFactors=FALSE),score_pattern(z$include,x,truth,z$exclude,
      constraint_count=1L+nzchar(z$exclude)))))
    logger("trace","content","score",sprintf("%s: scored %d include/exclude combinations; top bounded evidence FP/FN: %s.",label,nrow(evaluated),
      paste(head(paste0("#",seq_len(nrow(evaluated)),"=",evaluated$FP,"/",evaluated$FN),3L),collapse=", ")))
    reliable<-evaluated$Recall>=CONTENT_MIN_RECALL&evaluated$F1>=CONTENT_MIN_F1&evaluated$FP==0L
    pool<-if(any(reliable))evaluated[reliable,,drop=FALSE]else evaluated
    # With identical classification, prefer evidence-preserving abstraction to
    # a concrete literal.  A literal still wins whenever abstraction has an FP.
    pool$Generalization<-match(pool$Family,c("concrete","partial","shape","structural"),nomatch=0L)
    pool<-pool[order(pool$FN,pool$FP,-pool$BalancedAccuracy,-pool$Generalization,
      pool$BaseConstraints,pool$RegexLength,pool$Include,pool$Exclude),,drop=FALSE]
    base<-pool[1L,,drop=FALSE];includes<-base$Include;excludes<-base$Exclude[nzchar(base$Exclude)]
    refinement<-refine_pattern_search(base$Include,x,truth,base$Exclude)
    logger("trace","content","refine",sprintf("%s: refinement scored %d candidate(s), pruned %d; stop=%s.",label,
      nrow(refinement$candidates),sum(refinement$audit$Disposition=="rejected"),refinement$stop_reason))
    refinement_history[[label]]<-list(candidate_anchors=lapply(anchored,`[[`,"anchor_history"),refinement=refinement)
    # A refinement may replace the base only if it is reliable and preserves its
    # recall; redundancy is never permission to specialize a generalized class.
    if(!is.null(refinement$best)&&refinement$best$Tier=="reliable"&&refinement$best$Recall>=base$Recall) {
      includes<-refinement$best$Include;excludes<-refinement$best$Exclude[nzchar(refinement$best$Exclude)]
    }
    for(step in seq_len(max(0L,as.integer(redundancy)))) {
      current<-safe_grepl(includes[1L],x)
      if(length(includes)>1L)for(p in includes[-1L])current<-current&safe_grepl(p,x)
      if(length(excludes))for(p in excludes)current<-current&!safe_grepl(p,x)
      proposals<-list()
      existing_hits<-c(lapply(includes,function(p)safe_grepl(p,x)),lapply(excludes,function(p)!safe_grepl(p,x)))
      add<-function(type,p,hit) {
        independent<-!any(vapply(existing_hits,identical,logical(1),hit))
        applicable_ok<-all(hit[truth]);recall_ok<-sum((current&hit)&truth)>=sum(current&truth)
        information<-sum(current&!truth)-sum((current&hit)&!truth)
        near_neighbor<-any(current&!truth&!hit)
        if(independent&&applicable_ok&&recall_ok&&information>0L&&near_neighbor)
          proposals[[length(proposals)+1L]]<<-list(type=type,pattern=p,hit=current&hit,information=information)
      }
      for(p in setdiff(inc,includes))add("Include",p,safe_grepl(p,x))
      for(p in setdiff(exc,excludes))add("Exclude",p,!safe_grepl(p,x))
      if(!length(proposals))break
      o<-order(-vapply(proposals,`[[`,integer(1),"information"),vapply(proposals,function(z)nchar(z$pattern),integer(1)),vapply(proposals,`[[`,character(1),"pattern"))
      z<-proposals[[o[1L]]];before_fp<-sum(current&!truth);after_fp<-sum(z$hit&!truth)
      if(z$type=="Include")includes<-c(includes,z$pattern)else excludes<-c(excludes,z$pattern)
      redundancy_history<-rbind(redundancy_history,data.frame(Content=label,Step=step,Type=z$type,Pattern=z$pattern,
        InformationGain=z$information,EliminatedNegativeRows=paste(row_id[current&!truth&!z$hit],collapse=","),
        DeltaFalsePositives=after_fp-before_fp,DeltaRecall=0,Reason="independent stable constraint eliminated a current negative near-neighbor",stringsAsFactors=FALSE))
    }
    core<-if(length(includes)==1L)includes else paste0("(?=.*(?:",includes,"))",collapse="")
    exclude<-if(length(excludes))paste0("(?:",paste(excludes,collapse="|"),")")else ""
    m<-score_pattern(core,x,truth,exclude,constraint_count=length(includes)+length(excludes))
    # One positive is sufficient only when the persisted form replays with the
    # same complete-table TP/FP result.
    normalized_core<-regex_to_miraprot_storage(regex_from_miraprot_storage(core,"content"),"content")
    normalized_exclude<-regex_to_miraprot_storage(regex_from_miraprot_storage(exclude,"content"),"content")
    replay<-score_pattern(normalized_core,x,truth,normalized_exclude,
      constraint_count=length(includes)+length(excludes))
    replay_ok<-identical(as.integer(m[1L,c("TP","FP","FN")]),
      as.integer(replay[1L,c("TP","FP","FN")]))
    status<-if(replay_ok).refinement_tier(replay,FALSE)else"unresolved"
    core<-normalized_core;exclude<-normalized_exclude;m<-replay
    is_reliable<-status=="reliable"
    logger("debug","content","select",sprintf("%s: %s rule; include='%s', exclude='%s' (TP=%d, FP=%d, FN=%d); reason=%s.",
      label,status,.miraprot_safe_value(core),.miraprot_safe_value(exclude),m$TP,m$FP,m$FN,
      if(is_reliable)"quality thresholds met" else "quality thresholds not met"))
    statuses<-rbind(statuses,data.frame(Content=label,Status=status,Reliable=is_reliable,PositiveExamples=length(pos),NegativeExamples=length(neg),stringsAsFactors=FALSE))
    metrics<-rbind(metrics,cbind(data.frame(Content=label,Include=core,Exclude=exclude,stringsAsFactors=FALSE),m))
    hit<-safe_grepl(core,x);if(nzchar(exclude))hit<-hit&!safe_grepl(exclude,x)
    error_kinds<-c(rep("FP",min(3L,sum(hit&!truth))),rep("FN",min(3L,sum(!hit&truth))))
    error_rows<-c(head(row_id[hit&!truth],3L),head(row_id[!hit&truth],3L))
    if(length(error_rows))representatives<-rbind(representatives,data.frame(Content=rep(label,length(error_rows)),
      Kind=error_kinds,RowID=error_rows,stringsAsFactors=FALSE))
    if(is_reliable)rules<-rbind(rules,data.frame(Content=label,Include=core,Exclude=exclude,Transformation=transformation[[1L]],stringsAsFactors=FALSE,check.names=FALSE))
    else add_unresolved(label,"quality_threshold","best rule did not meet reliable precision/recall requirements")
  }
  rules<-rules[rules$Content!="Row Index"&rules$Include!="Row Index",,drop=FALSE]
  rules<-rbind(rules,data.frame(Content="Row Index",Include="Row Index",Exclude="",Transformation=NA_character_,stringsAsFactors=FALSE,check.names=FALSE))
  applied<-apply_content_table(df,rules)
  # Metrics must cover every expected class, including honest failures.  This
  # prevents aggregate diagnostics from silently treating omitted classes as
  # zero false negatives.
  missing_metrics<-setdiff(labels,metrics$Content)
  for(label in missing_metrics) {
    truth<-y==label;pattern<-if(identical(label,"Row Index"))"Row Index"else"(?!)"
    m<-score_pattern(pattern,x,truth)
    metrics<-rbind(metrics,cbind(data.frame(Content=label,
      Include=if(identical(label,"Row Index"))pattern else "",Exclude="",
      stringsAsFactors=FALSE),m))
  }
  pairwise<-data.frame()
  if(nrow(rules)>1L){hits<-sapply(seq_len(nrow(rules)),function(i)safe_grepl(rules$Include[i],x)&(!nzchar(rules$Exclude[i])|!safe_grepl(rules$Exclude[i],x)))
    for(i in seq_len(nrow(rules)-1L))for(j in (i+1L):nrow(rules)){rows<-which(hits[,i]&hits[,j]);if(length(rows))pairwise<-rbind(pairwise,data.frame(EarlierRule=i,LaterRule=j,EarlierContent=rules$Content[i],LaterContent=rules$Content[j],Rows=paste(rows,collapse=","),Count=length(rows),Winner=rules$Content[j],stringsAsFactors=FALSE))}}
  conflicts<-list(pairwise=pairwise,rows=applied$conflicts,application=applied$rows)
  final_summary<-content_assignment_summary(applied$rows,rules)
  logger("debug","content","conflicts",sprintf("Content application found %d overlapping row(s) across %d rule pair(s).",nrow(applied$conflicts),nrow(pairwise)))
  logger("info","content","infer",sprintf("Content inference completed: %d rules; assigned correctly=%d, false positives=%d, false negatives=%d, unresolved rows=%d, conflicts=%d, labels with no selected rule=%d.",
    nrow(rules),final_summary$assigned_correctly,final_summary$false_positives,
    final_summary$false_negatives,final_summary$unresolved_rows,final_summary$conflicts,
    final_summary$labels_with_no_selected_rule),details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  list(table=rules,status=statuses,metrics=metrics,refinement_history=refinement_history,
    redundancy_history=redundancy_history,conflicts=conflicts,unresolved_reasons=unresolved,
    representative_errors=representatives,summary=final_summary,warnings=unique(notes))
}
# ---- condition extraction ---------------------------------------------------
# This deliberately mirrors apply_condition_autoassign_dw(), rather than the
# similar (but observably different) preview helper in auto-assign utils.
extract_condition <- function(x, method, before="", after="", separators="", pos=1L) {
  x <- chr(x); n <- length(x); pos <- if (length(pos) && !is.na(pos)) as.integer(pos) else 1L
  before <- if (length(before) && !is.na(before)) before else ""
  after <- if (length(after) && !is.na(after)) after else ""
  separators <- if (length(separators) && !is.na(separators)) separators else ""
  cs <- function(pattern) if (nzchar(pattern) && !startsWith(pattern, "(?-i:")) paste0("(?-i:", pattern, ")") else pattern
  before_cs <- cs(before); after_cs <- cs(after)
  locate <- function(value, pattern) {
    if (!nzchar(pattern)) return(matrix(c(1L, 0L), nrow=1L, dimnames=list(NULL,c("start","end"))))
    stringr::str_locate_all(value, pattern)[[1L]]
  }
  one <- function(value) tryCatch(switch(method,
    between = {
      # Data Wizard wraps boundaries case-sensitively, but it does not require
      # lookbehind.  Locations also support safely-generalised variable-width
      # boundaries which PCRE lookbehind rejects.
      left <- locate(value, before_cs); right <- locate(value, after_cs)
      if (!nrow(left) || !nrow(right)) return(NA_character_)
      pairs <- do.call(rbind,lapply(seq_len(nrow(left)),function(i) {
        j<-which(right[,"start"]>left[i,"end"])[1L]
        if(length(j))c(left[i,"end"]+1L,right[j,"start"]-1L)else NULL
      }))
      if (is.null(pairs) || !nrow(pairs)) NA_character_ else substr(value,pairs[1L,1L],pairs[1L,2L])
    },
    start = {
      if (nzchar(after_cs)) {
        loc <- stringr::str_locate(value, after_cs)[1L, "start"]
        if (!is.na(loc) && loc > 1L) substr(value, 1L, loc - 1L) else value
      } else value
    },
    end = {
      if (nzchar(before_cs)) {
        locs <- stringr::str_locate_all(value, before_cs)[[1]]
        if (nrow(locs) >= 1L) {
          last <- locs[nrow(locs), "end"]
          if (last < nchar(value)) substr(value, last + 1L, nchar(value)) else ""
        } else value
      } else value
    },
    whole = value,
    phrase_position = {
      parts <- stringr::str_split(value, separators, simplify=TRUE)
      if (ncol(parts) >= pos) parts[pos] else NA_character_
    }, NA_character_), error=function(e) NA_character_)

  # pattern_detect is a batch operation: its eligible column is determined from
  # all rows of a content type, not independently for each column name.
  if (identical(method, "pattern_detect")) return(tryCatch({
    pieces <- strsplit(x, separators, perl=TRUE)
    width <- max(lengths(pieces)); mat <- t(vapply(pieces, function(p) { length(p)<-width; p }, character(width)))
    mat <- apply(mat, c(1,2), function(value) gsub("^[[:punct:]]+|[[:punct:]]+$", "", trimws(value)))
    if (is.null(dim(mat))) mat <- matrix(mat, nrow=n)
    eligible <- which(vapply(seq_len(ncol(mat)), function(j) { u<-unique(mat[,j]); length(u)>1L && length(u)<nrow(mat) }, logical(1)))
    if (length(eligible) >= pos) mat[,eligible[pos]] else rep(NA_character_, n)
  }, error=function(e) rep(NA_character_, n)))
  vapply(x, one, character(1))
}

condition_separators <- function(xs=character()) {
  # These are persisted regex atoms, not display characters.  In particular a
  # slash is stored as `\/`, a backslash as `\\`, and runs of observed white
  # space as `\s+`, exactly as the flow UI writes them.
  observed <- tokens(chr(xs)); text <- if(nrow(observed)) observed$Text[observed$Type!="identifier"] else character()
  atom <- function(z) if(grepl("^[[:space:]]+$",z,perl=TRUE)) "\\s+" else if(z=="/") "\\/" else regex_escape_literal(z)
  atoms <- unique(vapply(text,atom,character(1)))
  atoms <- atoms[nzchar(atoms)]
  if(!length(atoms)) return(character())
  # Preserve complete observed separator runs as well as their atoms.  The
  # Data Wizard persists one regex alternation, so e.g. `) / (` must remain a
  # usable alternative rather than being reduced to the punctuation singletons.
  runs<-unlist(regmatches(chr(xs),gregexpr("[[:space:]_/():-]+",chr(xs),perl=TRUE)),use.names=FALSE)
  run_pattern<-function(z) {
    pieces<-regmatches(z,gregexpr("[[:space:]]+|[^[:space:]]",z,perl=TRUE))[[1L]]
    paste0(vapply(pieces,atom,character(1)),collapse="")
  }
  sequences<-unique(vapply(runs[nzchar(runs)],run_pattern,character(1)))
  pairs<-if(length(atoms)>=2L)apply(combn(atoms,2L),2,paste,collapse="|")else character()
  alternatives<-unique(c(atoms,sequences))
  unique(c(alternatives,pairs,if(length(alternatives)>1L)paste(alternatives,collapse="|")else character()))
}

# Content assignment invokes set_ratio_or_identifier_options() immediately
# after setting Content.  These rows therefore receive Options="Ratio" before
# condition rules run and must not be treated as failed condition extraction.
AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT <- c("Abundance Ratio", "Abundance Ratio p-Value",
  "Abundance Ratio Adj. p-Value")

condition_contexts <- function(xs, ys, side, max_width=CONDITION_CONTEXT_SEARCH_WIDTH) {
  values <- character()
  for (i in seq_along(xs)) {
    hits <- gregexpr(ys[i], xs[i], fixed=TRUE)[[1L]]
    hits <- hits[hits>0L]
    if (length(hits)!=1L || !nzchar(ys[i])) next
    loc <- hits[[1L]]
    raw <- if (side=="before") substr(xs[i],1L,loc-1L) else substr(xs[i],loc+nchar(ys[i]),nchar(xs[i]))
    if (!nzchar(raw)) next
    widths <- seq_len(min(nchar(raw), max_width))
    fragments <- if (side=="before") substring(raw,nchar(raw)-widths+1L) else substring(raw,1L,widths)
    values <- c(values, regex_to_miraprot_storage(regex_atom_for_token(fragments), "condition_boundary"))
    # Preserve the punctuation nearest the label and generalise only a complete
    # adjacent identifier.  Partial identifiers are never replaced.
    z<-tokens(raw); if(nrow(z)) {
      rows<-if(side=="before")rev(seq_len(nrow(z)))else seq_len(nrow(z))
      kept<-character()
      for(j in rows) {
        a<-if(z$Type[j]=="identifier") "[[:alnum:]]+" else if(z$Type[j]=="whitespace") "\\s+" else regex_escape_literal(z$Text[j])
        kept<-if(side=="before")c(a,kept)else c(kept,a)
        candidate<-paste0(kept,collapse="")
        if(nchar(candidate)<=max_width+12L)values<-c(values,candidate)
      }
    }
  }
  values<-unique(values[nzchar(values)])
  # A boundary which is absent from even one applicable row cannot possibly
  # produce a complete rule.  Prefer short, literal contexts before structural
  # generalisations so subsequent bounded products remain deterministic.
  present_everywhere<-vapply(values,function(value)all(vapply(xs,function(x)
    any(!is.na(stringr::str_locate_all(x,paste0("(?-i:",value,")"))[[1L]][,"start"])),logical(1))),logical(1))
  values<-values[present_everywhere]
  values[order(nchar(values),grepl("[[:",values,fixed=TRUE),values,method="radix")]
}

infer_conditions <- function(df,target,logger=function(...) invisible(NULL)) {
  started<-proc.time()[["elapsed"]]
  logger("debug", "condition", "infer", sprintf("Inferring condition rules from %d rows.", nrow(df)),
         details=list(examples=head(chr(df$Column), MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)))
  out<-empty_condition(); diagnostic_blocks<-list(); warnings<-character()
  if(!nzchar(target)||!target%in%names(df)) return(list(table=out,diagnostics=data.frame(),warnings="Condition target unavailable."))
  labels <- unique(chr(df$Content)[nzchar(chr(df$Content))])
  simplicity <- c(whole=1L,start=2L,end=2L,phrase_position=3L,between=4L,pattern_detect=5L)
  statuses<-data.frame(); unresolved<-data.frame()
  for(label in labels) {
    label_started<-proc.time()[["elapsed"]]
    group_idx <- chr(df$Content)==label
    group_rows <- which(group_idx)
    references <- chr(df[[target]][group_idx])
    available <- nzchar(references)
    unavailable_rows <- group_rows[!available]
    if (!is_sample_bearing_content(label)) {
      note <- sprintf("Content type '%s' is not sample-bearing; condition inference was skipped", label)
      statuses<-rbind(statuses,data.frame(Content=label,SelectedMethod="",Before="",After="",Separators="",Pos=1L,Status="not_applicable",ExactMatches=0L,IncorrectNonempty=0L,EmptyResults=0L,Ambiguities=0L,ConstantOutputFailure=FALSE,UnavailableReferences=length(unavailable_rows),UnresolvedReason=note,stringsAsFactors=FALSE))
      logger("debug","condition","select",note)
      next
    }
    if(!any(available)) {
      note <- "No reference condition is available; condition inference was skipped"
      warnings<-c(warnings,paste0(label,": ",note,"."))
      statuses<-rbind(statuses,data.frame(Content=label,SelectedMethod="",Before="",After="",Separators="",Pos=1L,Status="not_applicable",ExactMatches=0L,IncorrectNonempty=0L,EmptyResults=0L,Ambiguities=0L,ConstantOutputFailure=FALSE,UnavailableReferences=length(unavailable_rows),UnresolvedReason="",stringsAsFactors=FALSE))
      diagnostic_blocks[[length(diagnostic_blocks)+1L]]<-data.frame(Content=label,CandidateRank=NA_integer_,Method="",Before="",After="",Separators="",Pos=NA_integer_,Row=unavailable_rows,Column=chr(df$Column[unavailable_rows]),PredictedCondition="",ExpectedCondition="",ReferenceAvailable=FALSE,EmptyExtraction=NA,IncorrectNonempty=NA,Ambiguous=NA,ExactMatch=NA,ExactMatches=NA_integer_,IncorrectNonemptyResults=NA_integer_,EmptyResults=NA_integer_,Ambiguities=NA_integer_,ConstantOutputFailure=FALSE,CompleteRowAccuracy=NA_real_,MethodSimplicity=NA_integer_,stringsAsFactors=FALSE)
      next
    }
    idx <- group_idx & nzchar(chr(df[[target]])); xs<-chr(df$Column[idx]); ys<-chr(df[[target]][idx])
    if(length(unavailable_rows)) warnings<-c(warnings,sprintf("%s: %d row(s) have unavailable condition references and were excluded from inference.",label,length(unavailable_rows)))
    candidates <- list();candidate_predictions<-list()
    prediction_index<-new.env(hash=TRUE,parent=emptyenv())
    prediction_key<-function(prediction) paste0(ifelse(is.na(prediction),"N",
      paste0("V",nchar(prediction,type="bytes"),":",prediction)),collapse="\035")
    generated<-0L;pruned<-0L;early_reason<-"all method tiers required"
    add <- function(method,before="",after="",separators="",pos=1L) {
      generated<<-generated+1L
      candidate<-data.frame(Method=method,Before=before,After=after,Separators=separators,Pos=as.integer(pos),stringsAsFactors=FALSE)
      key<-paste(candidate,collapse="\034")
      existing<-if(length(candidates))vapply(candidates,function(z)paste(z,collapse="\034"),character(1))else character()
      if(key%in%existing){pruned<<-pruned+1L;return(invisible(FALSE))}
      prediction<-extract_condition(xs,method,before,after,separators,pos)
      prediction_string<-prediction_key(prediction)
      if(exists(prediction_string,envir=prediction_index,inherits=FALSE)){pruned<<-pruned+1L;return(invisible(FALSE))}
      candidate_number<-length(candidates)+1L
      candidates[[candidate_number]] <<- candidate
      candidate_predictions[[candidate_number]] <<- prediction
      assign(prediction_string,candidate_number,envir=prediction_index)
      invisible(TRUE)
    }
    tier_exact<-function(from) {
      if(length(candidates)<from)return(FALSE)
      any(vapply(candidate_predictions[from:length(candidate_predictions)],function(p){
        all(!is.na(p)&nzchar(p)&p==ys) && !(length(unique(p))<=1L&&length(unique(ys))>1L)},logical(1)))
    }
    add("whole");tier_start<-1L
    stop_search<-tier_exact(tier_start)
    befores<-afters<-character()
    if(!stop_search) {
      befores<-head(condition_contexts(xs,ys,"before"),CONDITION_BOUNDARY_LIMIT)
      afters<-head(condition_contexts(xs,ys,"after"),CONDITION_BOUNDARY_LIMIT)
      tier_start<-length(candidates)+1L
      for(a in afters)add("start",after=a)
      for(b in befores)add("end",before=b)
      stop_search<-tier_exact(tier_start)
      if(stop_search)early_reason<-"exact nonambiguous start/end tier established"
    } else early_reason<-"exact nonambiguous whole-string tier established"
    separators<-condition_separators(xs)
    if(!stop_search) {
      tier_start<-length(candidates)+1L
      for(s in separators){widths<-lengths(strsplit(xs,s,perl=TRUE));for(p in seq_len(max(widths)))add("phrase_position",separators=s,pos=p)}
      stop_search<-tier_exact(tier_start)
      if(stop_search)early_reason<-"exact nonambiguous phrase-position tier established"
    }
    if(!stop_search) {
      if(!length(befores))befores<-head(condition_contexts(xs,ys,"before"),CONDITION_BOUNDARY_LIMIT)
      if(!length(afters))afters<-head(condition_contexts(xs,ys,"after"),CONDITION_BOUNDARY_LIMIT)
      product<-expand.grid(b=befores,a=afters,stringsAsFactors=FALSE)
      if(nrow(product)>CONDITION_BOUNDARY_PAIR_LIMIT){pruned<-pruned+nrow(product)-CONDITION_BOUNDARY_PAIR_LIMIT;product<-head(product,CONDITION_BOUNDARY_PAIR_LIMIT)}
      tier_start<-length(candidates)+1L
      for(j in seq_len(nrow(product)))add("between",before=product$b[j],after=product$a[j])
      stop_search<-tier_exact(tier_start)
      if(stop_search)early_reason<-"exact nonambiguous bounded boundary tier established"
    }
    if(!stop_search)for(s in separators){widths<-lengths(strsplit(xs,s,perl=TRUE));for(p in seq_len(max(widths)))add("pattern_detect",separators=s,pos=p)}
    cand <- unique(do.call(rbind,candidates)); pruned<-pruned+length(candidates)-nrow(cand);rownames(cand)<-NULL
    # Score the representation that users will actually load, not a richer
    # in-memory object which may accidentally survive coercion differently.
    candidate_path<-tempfile(fileext=".rds");saveRDS(cand,candidate_path)
    cand<-readRDS(candidate_path);unlink(candidate_path)
    logger("trace","condition","boundaries",sprintf("%s: candidates generated=%d; pruned before scoring=%d; candidates scored=%d; early stop=%s; cache hits=%d; boundary examples: %s.",
      label,generated,pruned,nrow(cand),early_reason,0L,paste(head(unique(c(cand$Before,cand$After,cand$Separators))[nzchar(unique(c(cand$Before,cand$After,cand$Separators)))],3L),collapse=" | ")))
    predictions <- candidate_predictions
    exact_count <- vapply(predictions,function(p) sum(!is.na(p)&p==ys),integer(1))
    incorrect_count <- vapply(predictions,function(p) sum(!is.na(p)&nzchar(p)&p!=ys),integer(1))
    accuracy <- exact_count/length(ys)
    empty_count <- vapply(predictions,function(p) sum(is.na(p)|!nzchar(p)),integer(1))
    constant_failure <- vapply(predictions,function(p) length(unique(p[!is.na(p)&nzchar(p)]))<=1L && length(unique(ys))>1L,logical(1))
    complexity <- nchar(cand$Before)+nchar(cand$After)+nchar(cand$Separators)+ifelse(is.na(cand$Pos),0L,cand$Pos)
    ord<-order(-accuracy,simplicity[cand$Method],complexity,cand$Method,cand$Before,cand$After,cand$Separators,ifelse(is.na(cand$Pos),0L,cand$Pos))
    ranks<-integer(nrow(cand)); ranks[ord]<-seq_along(ord)
    ambiguous_rows<-lapply(seq_along(predictions),function(i) {
      z<-cand[i,]; if(z$Method=="between")vapply(xs,function(value){l<-stringr::str_locate_all(value,paste0("(?-i:",z$Before,")"))[[1]];r<-stringr::str_locate_all(value,paste0("(?-i:",z$After,")"))[[1]];nrow(l)>1L||nrow(r)>1L},logical(1))else rep(FALSE,length(xs))
    })
    ambiguity_count<-vapply(ambiguous_rows,sum,integer(1))
    for(i in seq_len(nrow(cand))) {
      p<-predictions[[i]]
      mismatch<-ifelse(is.na(p)|!nzchar(p),"empty extraction",ifelse(p==ys,"",sprintf("predicted '%s' instead of '%s'",p,ys)))
      diagnostic_blocks[[length(diagnostic_blocks)+1L]]<-data.frame(Content=label,CandidateRank=ranks[i],Method=cand$Method[i],Before=cand$Before[i],After=cand$After[i],Separators=cand$Separators[i],Pos=cand$Pos[i],Row=which(idx),Column=xs,PredictedCondition=ifelse(is.na(p),"",p),ExpectedCondition=ys,ReferenceAvailable=TRUE,EmptyExtraction=is.na(p)|!nzchar(p),IncorrectNonempty=!is.na(p)&nzchar(p)&p!=ys,Ambiguous=ambiguous_rows[[i]],ExactMatch=!is.na(p)&p==ys,ExactMatches=exact_count[i],IncorrectNonemptyResults=incorrect_count[i],EmptyResults=empty_count[i],Ambiguities=ambiguity_count[i],ConstantOutputFailure=constant_failure[i],CompleteRowAccuracy=accuracy[i],MethodSimplicity=unname(simplicity[cand$Method[i]]),TopCandidateMethod=cand$Method[ord[1L]],TopPredictedCondition=ifelse(is.na(predictions[[ord[1L]]]),"",predictions[[ord[1L]]]),ExactMismatchReason=mismatch,stringsAsFactors=FALSE)
    }
    if(length(unavailable_rows)) diagnostic_blocks[[length(diagnostic_blocks)+1L]]<-data.frame(Content=label,CandidateRank=NA_integer_,Method="",Before="",After="",Separators="",Pos=NA_integer_,Row=unavailable_rows,Column=chr(df$Column[unavailable_rows]),PredictedCondition="",ExpectedCondition="",ReferenceAvailable=FALSE,EmptyExtraction=NA,IncorrectNonempty=NA,Ambiguous=NA,ExactMatch=NA,ExactMatches=NA_integer_,IncorrectNonemptyResults=NA_integer_,EmptyResults=NA_integer_,Ambiguities=NA_integer_,ConstantOutputFailure=FALSE,CompleteRowAccuracy=NA_real_,MethodSimplicity=NA_integer_,stringsAsFactors=FALSE)
    eligible<-ord[accuracy[ord]==1&empty_count[ord]==0L&ambiguity_count[ord]==0L&!constant_failure[ord]]
    best<-if(length(eligible))eligible[1L]else ord[1L]; reliable<-length(eligible)>0L
    logger("trace","condition","score",sprintf("%s tie-break: accuracy %.3f, simplicity %d, complexity %d, rank %d.",
      label,accuracy[best],simplicity[cand$Method[best]],complexity[best],ranks[best]))
    logger("debug","condition","select",sprintf("%s: %s method '%s'; %d/%d exact; reason=%s; elapsed=%.1f ms.",label,
      if(reliable)"selected" else "rejected",cand$Method[best],exact_count[best],length(ys),
      if(reliable)"exact nonambiguous extraction" else "no exact nonambiguous candidate",
      (proc.time()[["elapsed"]]-label_started)*1000),details=list(selected_method=if(reliable)cand$Method[best]else"",elapsed_ms=(proc.time()[["elapsed"]]-label_started)*1000))
    if(reliable) out<-rbind(out,data.frame(Content=label,Method=cand$Method[best],Before=cand$Before[best],After=cand$After[best],Separators=cand$Separators[best],Pos=cand$Pos[best],check.names=FALSE))
    reason<-if(reliable)""else sprintf("best supported method matched %.1f%%; %d incorrect nonempty, %d empty, %d ambiguous",100*accuracy[best],incorrect_count[best],empty_count[best],ambiguity_count[best])
    statuses<-rbind(statuses,data.frame(Content=label,SelectedMethod=if(reliable)cand$Method[best]else "",Before=if(reliable)cand$Before[best]else "",After=if(reliable)cand$After[best]else "",Separators=if(reliable)cand$Separators[best]else "",Pos=if(reliable)cand$Pos[best]else 1L,Status=if(reliable)"reliable"else"unresolved",ExactMatches=exact_count[best],IncorrectNonempty=incorrect_count[best],EmptyResults=empty_count[best],Ambiguities=ambiguity_count[best],ConstantOutputFailure=constant_failure[best],UnavailableReferences=length(unavailable_rows),UnresolvedReason=reason,stringsAsFactors=FALSE))
    if(!reliable){unresolved<-rbind(unresolved,data.frame(Content=label,Reason=reason,stringsAsFactors=FALSE));warnings<-c(warnings,paste(label,"condition extraction unresolved:",reason))}
    if(!reliable && length(xs)==1L) logger("trace","condition","select",sprintf(
      "%s unresolved single row: source='%s'; expected='%s'; predicted='%s'; reason=%s.",label,
      .miraprot_safe_value(xs,160L),.miraprot_safe_value(ys,120L),
      .miraprot_safe_value(predictions[[best]],120L),.miraprot_safe_value(reason,200L)))
  }
  if(length(diagnostic_blocks)) {
    diagnostic_names<-unique(unlist(lapply(diagnostic_blocks,names),use.names=FALSE))
    diagnostic_blocks<-lapply(diagnostic_blocks,function(block) {
      for(name in setdiff(diagnostic_names,names(block))) block[[name]]<-NA
      block[,diagnostic_names,drop=FALSE]
    })
  }
  diag<-if(length(diagnostic_blocks)) do.call(rbind,diagnostic_blocks) else data.frame()
  if(nrow(diag)) diag<-diag[order(diag$Content,diag$CandidateRank,diag$Row),,drop=FALSE]
  # Completion gate: replay every emitted row through the public application
  # path.  A rule is suppressed if serialization/application changes its claim.
  if(nrow(out)) {
    replay_rows<-nzchar(chr(df[[target]])) & chr(df$Content)%in%out$Content
    path<-tempfile(fileext=".rds");on.exit(unlink(path),add=TRUE)
    saveRDS(out,path);serialized_out<-readRDS(path);unlink(path)
    replay<-apply_condition_table(data.frame(Column=chr(df$Column[replay_rows]),Content=chr(df$Content[replay_rows]),stringsAsFactors=FALSE),serialized_out,chr(df[[target]][replay_rows]))
    bad<-unique(replay$diagnostics$Content[replay$diagnostics$Content%in%out$Content&!replay$diagnostics$ExactMatch])
    if(length(bad)){out<-out[!out$Content%in%bad,,drop=FALSE];statuses$Status[statuses$Content%in%bad]<-"unresolved";statuses$UnresolvedReason[statuses$Content%in%bad]<-"completion-gate replay failed";unresolved<-rbind(unresolved,data.frame(Content=bad,Reason="completion-gate replay failed",stringsAsFactors=FALSE));warnings<-c(warnings,paste(bad,"condition extraction unresolved: completion-gate replay failed"))}
  }
  logger("info","condition","infer",sprintf("Condition inference completed: %d rules and %d unresolved labels.",nrow(out),nrow(unresolved)),
    details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  list(table=out,status=statuses,diagnostics=diag,unresolved_reasons=unresolved,warnings=unique(warnings))
}

# ---- ratio inference --------------------------------------------------------
ratio_between <- function(x, before=NA_character_, after=NA_character_) {
  tryCatch({
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    start <- 1L; finish <- nchar(x)
    if (!is.na(before) && nzchar(before)) {
      loc <- stringr::str_locate(x, before)[1L,]
      if (is.na(loc[1L])) return(NA_character_)
      start <- loc[2L] + 1L
    }
    if (!is.na(after) && nzchar(after)) {
      loc <- stringr::str_locate(substr(x,start,nchar(x)), after)[1L,]
      if (is.na(loc[1L])) return(NA_character_)
      finish <- start + loc[1L] - 2L
    }
    if (finish < start) return(NA_character_)
    value <- trimws(substr(x,start,finish)); if (nzchar(value)) value else NA_character_
  }, error=function(e) NA_character_)
}

# These helpers intentionally reproduce datawizard_auto_assign_utils.R rather
# than implementing more conventional split/regex behaviour.
ratio_normalize_boundaries <- function(nb,na,db,da) {
  clean <- function(x) if (length(x) && !is.na(x) && nzchar(x)) as.character(x) else NA_character_
  nb<-clean(nb); na<-clean(na); db<-clean(db); da<-clean(da)
  if (is.na(na) && is.na(db)) return(list(skip=TRUE))
  if (is.na(na)) na<-db
  if (is.na(db)) db<-na
  list(skip=FALSE,nb=nb,na=na,db=db,da=da)
}
ratio_tokenize_exact <- function(x,separators=NA_character_) {
  cache<-getOption("miraprot.ratio_token_cache",NULL)
  sep_key<-if(length(separators)&&!is.na(separators)&&nzchar(separators))as.character(separators)else"<default>"
  key<-paste0(enc2utf8(x),"\034",sep_key)
  if(is.environment(cache)&&exists(key,envir=cache,inherits=FALSE)){
    cache$hits<-cache$hits+1L
    return(get(key,envir=cache,inherits=FALSE))
  }
  if (is.na(x)||!nzchar(x)) {
    result<-character();if(is.environment(cache))assign(key,result,envir=cache)
    return(result)
  }
  sep<-if(length(separators)&&!is.na(separators)&&nzchar(separators)) separators else "\\s+|\\(|\\)|\\[|\\]|\\{|\\}|/|_|-"
  locs<-tryCatch(stringr::str_locate_all(x,paste0("(",sep,")"))[[1]],error=function(e)matrix(numeric(),ncol=2))
  if(!nrow(locs)){z<-trimws(x);result<-if(nzchar(z))z else character();if(is.environment(cache))assign(key,result,envir=cache);return(result)}
  starts<-c(1L,locs[,"end"]+1L); ends<-c(locs[,"start"]-1L,nchar(x)); keep<-starts<=ends
  starts<-starts[keep];ends<-ends[keep]; atomic<-character();ranges<-list()
  for(i in seq_along(starts)){z<-trimws(substr(x,starts[i],ends[i]));if(nzchar(z)&&(!length(atomic)||tail(atomic,1)!=z)){atomic<-c(atomic,z);ranges[[length(ranges)+1L]]<-c(starts[i],ends[i])}}
  grams<-character();n<-length(atomic);if(n>=2L)for(len in 2L:n)for(i in seq_len(n-len+1L))grams<-c(grams,trimws(substr(x,ranges[[i]][1],ranges[[i+len-1L]][2])))
  result<-unique(c(atomic,grams[nzchar(grams)]))
  if(is.environment(cache))assign(key,result,envir=cache)
  result
}
ratio_extract <- function(x,rule,known=character()) {
  scalar<-function(name,default=NA)if(name%in%names(rule))rule[[name]][1L]else default
  method<-as.character(scalar("Method","")); invert<-isTRUE(scalar("Invert",FALSE)); num<-den<-NA_character_
  if(method=="Regular Expressions") {
    z<-ratio_normalize_boundaries(scalar("NumBefore"),scalar("NumAfter"),scalar("DenBefore"),scalar("DenAfter"))
    if(isTRUE(z$skip)) return(NULL)
    num<-ratio_between(x,z$nb,z$na);den<-ratio_between(x,z$db,z$da)
  } else if(method=="Position in String") {
    toks<-ratio_tokenize_exact(x,scalar("Separators"))
    at<-function(p)if(!is.na(p)&&p>=1L&&p<=length(toks))toks[[p]]else NA_character_
    num<-at(as.integer(scalar("NumPos")));den<-at(as.integer(scalar("DenPos")))
  } else if(method=="Pattern Recognition") {
    toks<-ratio_tokenize_exact(x,scalar("Separators"))
    toks<-toks[!grepl("(?i)^(abundance|ratio|normalized|raw|adj\\.?|p-?value|variability|mean|median|value|stat(istic)?s?)$",toks,perl=TRUE)]
    picks<-character();seen<-character()
    for(tok in toks)if(tok%in%known&&!tok%in%seen){picks<-c(picks,tok);seen<-c(seen,tok);if(length(picks)==2L)break}
    if(length(picks)<2L){pm<-stringr::str_match_all(x,"\\(([^)]*)\\)")[[1]];if(nrow(pm))picks<-unique(c(picks,trimws(pm[,2L])[nzchar(trimws(pm[,2L]))]))}
    if(length(picks))num<-picks[1L];if(length(picks)>=2L)den<-picks[2L]
  }
  if(invert&&!is.na(num)&&!is.na(den)){tmp<-num;num<-den;den<-tmp}
  if(is.na(num)&&is.na(den))NULL else list(numerator=num,denominator=den)
}

known_samples_after_conditions <- function(metadata) {
  # Data Wizard builds the recognition dictionary from the assignment column
  # after condition rules have run.  Keep this in one place: inference and the
  # exported-rule executor must never independently reinterpret Options.
  if (!is.data.frame(metadata) || !all(c("Content","Options") %in% names(metadata))) return(character())
  values <- trimws(chr(metadata$Options[is_sample_bearing_content(metadata$Content)]))
  unique(values[!is.na(values) & nzchar(values)])
}

ratio_diagnostics <- function(label,rows,xs,ns,ds,rule,known,content_targeted=TRUE) {
  got<-lapply(xs,ratio_extract,rule=rule,known=known)
  pn<-vapply(got,function(z)if(is.null(z))NA_character_ else z$numerator,character(1));pd<-vapply(got,function(z)if(is.null(z))NA_character_ else z$denominator,character(1))
  applicable<-nzchar(ns)&nzchar(ds);ok<-applicable&!is.na(pn)&!is.na(pd)&pn==ns&pd==ds
  reason<-ifelse(ok,"",ifelse(vapply(got,is.null,logical(1)),"No components extracted",ifelse(is.na(pn)|!nzchar(pn),"Numerator was not extracted",ifelse(is.na(pd)|!nzchar(pd),"Denominator was not extracted",ifelse(pn!=ns,"Numerator mismatch","Denominator mismatch")))))
  reason[!applicable]<-"Target pair is incomplete"
  data.frame(Content=label,Row=rows,Column=xs,Method=rule$Method,
    LocalNumerator=ifelse(is.na(pn),"",pn),LocalDenominator=ifelse(is.na(pd),"",pd),
    PredictedNumerator=ifelse(is.na(pn),"",pn),ExpectedNumerator=ns,
    PredictedDenominator=ifelse(is.na(pd),"",pd),ExpectedDenominator=ds,
    ExpectedComponents=paste(ns,ds,sep=" / "),KnownSampleCount=length(known),
    ApplicableContentTargeted=rep(content_targeted,length(rows)),
    ReplayNumerator="",ReplayDenominator="",ReplaySuccess=FALSE,ReplayFailureReason="Not replayed",
    Applicable=applicable,Success=ok,FailureReason=reason,stringsAsFactors=FALSE)
}

infer_ratios <- function(df,logger=function(...) invisible(NULL)) {
  started<-proc.time()[["elapsed"]]
  token_cache<-new.env(parent=emptyenv());token_cache$hits<-0L
  old_token_cache<-getOption("miraprot.ratio_token_cache",NULL)
  options(miraprot.ratio_token_cache=token_cache)
  on.exit(options(miraprot.ratio_token_cache=old_token_cache),add=TRUE)
  logger("debug", "ratio", "infer", sprintf("Inferring ratio rules from %d rows.", nrow(df)),
         details=list(examples=head(chr(df$Column), MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)))
  out<-empty_ratio();diag_blocks<-list();statuses<-data.frame();warnings<-character();if(!all(c("Numerator","Denominator")%in%names(df)))return(list(table=out,status=statuses,diagnostics=data.frame(),warnings="Ratio targets unavailable."))
  valid<-nzchar(chr(df$Numerator))&nzchar(chr(df$Denominator))
  # Construct the same precursor state used by application.  Reference Content
  # is retained separately below only for the isolated extraction check.
  content_rules<-if("Content"%in%names(df))infer_content(df)$table else empty_content()
  condition_rules<-if("Options"%in%names(df))infer_conditions(df,"Options")$table else empty_condition()
  pipeline_metadata<-df
  if("Content"%in%names(df))pipeline_metadata<-apply_content_table(pipeline_metadata,content_rules)$metadata
  pipeline_metadata<-apply_condition_table(pipeline_metadata,condition_rules)$metadata
  known<-known_samples_after_conditions(pipeline_metadata)
  # Stage 1 deliberately uses the mapped reference condition values.  It asks
  # whether the persisted ratio rule itself works when its Content target is
  # correct, independently of content/condition inference.
  replay_known<-known_samples_after_conditions(df)
  blank_rule<-function(label,method,sep=NA_character_,inv=FALSE,nb=NA_character_,na=NA_character_,db=NA_character_,da=NA_character_,np=NA_integer_,dp=NA_integer_)data.frame(Content=label,Method=method,Separators=sep,Invert=inv,NumBefore=nb,NumAfter=na,DenBefore=db,DenAfter=da,NumPos=as.integer(np),DenPos=as.integer(dp),stringsAsFactors=FALSE,check.names=FALSE)
  for(label in sort(unique(chr(df$Content[valid])))){
    label_started<-proc.time()[["elapsed"]]
    idx<-valid&chr(df$Content)==label;rows<-which(idx);xs<-chr(df$Column[idx]);ns<-chr(df$Numerator[idx]);ds<-chr(df$Denominator[idx]);candidates<-list();generated<-0L;pruned<-0L
    prediction_keys<-character()
    add<-function(rule){
      generated<<-generated+1L
      got<-lapply(xs,ratio_extract,rule=rule,known=replay_known)
      key<-paste(rule$Method,paste(vapply(got,function(z)if(is.null(z))"<none>"else paste(z$numerator,z$denominator,sep="\035"),character(1)),collapse="\034"),sep="\036")
      if(key%in%prediction_keys){pruned<<-pruned+1L;return(invisible(FALSE))}
      prediction_keys<<-c(prediction_keys,key);candidates[[length(candidates)+1L]]<<-rule;invisible(TRUE)
    }
    observed<-condition_separators(xs);fallback<-"\\s+|\\(|\\)|\\[|\\]|\\{|\\}|\\/|_|-"
    seps<-unique(c(observed,if(length(observed))paste(unique(unlist(strsplit(observed,"|",fixed=TRUE))),collapse="|")else character(),fallback))
    seps<-seps[order(nchar(seps),seps,method="radix")]
    # Pattern Recognition is only useful with mapped samples (or the documented
    # parentheses fallback); replay below decides whether either claim is true.
    for(s in seps)for(inv in c(FALSE,TRUE))add(blank_rule(label,"Pattern Recognition",s,inv))
    # Position rules use only separator alternations which can actually be
    # persisted and replayed, and positions are one based.
    for(s in seps){toks<-lapply(xs,ratio_tokenize_exact,separators=s);width<-max(0L,lengths(toks));if(width)for(inv in c(FALSE,TRUE)){
      raw_n<-if(inv)ds else ns;raw_d<-if(inv)ns else ds
      capable<-function(target)which(vapply(seq_len(width),function(p)all(vapply(seq_along(toks),function(i)length(toks[[i]])>=p&&identical(toks[[i]][p],target[i]),logical(1))),logical(1)))
      npos<-capable(raw_n);dpos<-capable(raw_d)
      pruned<-pruned+(width*width-length(npos)*length(dpos))
      for(np in npos)for(dp in dpos)add(blank_rule(label,"Position in String",s,inv,np=np,dp=dp))
    }}
    simple_complete<-which(vapply(candidates,function(rule){d<-ratio_diagnostics(label,rows,xs,ns,ds,rule,replay_known)
      if(!all(d$Success))return(FALSE)
      normalized<-coerce_contract(list(table=empty_content(),condition=empty_condition(),ratio=rule))$ratio
      direct<-apply_ratio_table(transform(df[idx,,drop=FALSE],Content=label),normalized,ns,ds)
      all(direct$diagnostics$Success)},logical(1)))
    preferred<-simple_complete[vapply(candidates[simple_complete],function(r)r$Method=="Position in String",logical(1))]
    stop_before_regex<-length(preferred)>0L
    # Infer regex boundaries from the aligned context of the raw (pre-invert)
    # component.  Candidate pairs must reproduce that component on every row.
    contexts<-function(target,side){z<-NA_character_;for(i in seq_along(xs)){hits<-gregexpr(target[i],xs[i],fixed=TRUE)[[1L]];hits<-hits[hits>0L];if(length(hits)==1L){raw<-if(side=="before")substr(xs[i],1L,hits-1L)else substr(xs[i],hits+nchar(target[i]),nchar(xs[i]));if(nzchar(raw)){w<-seq_len(min(RATIO_CONTEXT_SEARCH_WIDTH,nchar(raw)));fr<-if(side=="before")substring(raw,nchar(raw)-w+1L)else substring(raw,1L,w);z<-c(z,regex_to_miraprot_storage(regex_atom_for_token(fr),"ratio_boundary"))}}};z<-unique(z);z<-z[order(nchar(ifelse(is.na(z),"",z)),ifelse(is.na(z),"",z),na.last=TRUE)];head(z,RATIO_CONTEXT_LIMIT)}
    pairs<-function(target){bs<-contexts(target,"before");as<-contexts(target,"after");allp<-unlist(lapply(bs,function(b)lapply(as,function(a)list(b,a))),recursive=FALSE);if(length(allp)>RATIO_CONTEXT_PAIR_LIMIT){pruned<<-pruned+length(allp)-RATIO_CONTEXT_PAIR_LIMIT;allp<-head(allp,RATIO_CONTEXT_PAIR_LIMIT)};Filter(function(p){v<-mapply(ratio_between,xs,MoreArgs=list(before=p[[1]],after=p[[2]]));all(!is.na(v)&v==target)},allp)}
    if(!stop_before_regex)for(inv in c(FALSE,TRUE)){raw_n<-if(inv)ds else ns;raw_d<-if(inv)ns else ds;npairs<-pairs(raw_n);dpairs<-pairs(raw_d);product_count<-length(npairs)*length(dpairs);limit<-min(product_count,RATIO_CONTEXT_PAIR_LIMIT);if(product_count>limit)pruned<-pruned+product_count-limit;k<-0L;for(nq in npairs)for(dq in dpairs){k<-k+1L;if(k>limit)break;z<-ratio_normalize_boundaries(nq[[1]],nq[[2]],dq[[1]],dq[[2]]);if(!isTRUE(z$skip))add(blank_rule(label,"Regular Expressions",inv=inv,nb=nq[[1]],na=nq[[2]],db=dq[[1]],da=dq[[2]]))}}
    evaluated<-lapply(seq_along(candidates),function(i){d<-ratio_diagnostics(label,rows,xs,ns,ds,candidates[[i]],replay_known);d$Candidate<-i;d})
    method_counts<-table(vapply(candidates,function(r)r$Method,character(1)))
    logger("trace","ratio","components",sprintf("%s: candidates generated=%d; pruned before scoring=%d; candidates scored=%d; early stop=%s; cache hits=%d; methods: %s; row IDs %s.",label,
      generated,pruned,length(candidates),if(stop_before_regex)"complete replaying Position in String tier established"else"regular-expression fallback required",token_cache$hits,paste(names(method_counts),method_counts,sep="=",collapse=", "),paste(head(rows,3L),collapse=",")))
    scores<-vapply(evaluated,function(d)sum(d$Success),integer(1));complete<-length(rows)>=1L&scores==length(rows)
    complexity<-vapply(candidates,function(r)sum(nchar(chr(unlist(r[c("Separators","NumBefore","NumAfter","DenBefore","DenAfter")]))))+sum(c(r$NumPos,r$DenPos),na.rm=TRUE),numeric(1))
    # Equal complete fits favour methods which do not depend on condition
    # context.  Pattern Recognition remains available, but carries replay risk
    # that positional and boundary rules do not.
    simplicity<-match(vapply(candidates,function(r)r$Method,character(1)),c("Position in String","Regular Expressions","Pattern Recognition"))
    stored_value<-function(r,n){z<-chr(r[[n]])[1L];if(is.na(z))"" else z}
    # Rank only by persisted values after method simplicity and complexity.  The
    # final candidate number makes otherwise identical rules stable.
    ord<-do.call(order,c(list(
      !complete,simplicity,complexity,
      vapply(candidates,stored_value,character(1),"Separators"),
      vapply(candidates,stored_value,character(1),"NumBefore"),
      vapply(candidates,stored_value,character(1),"NumAfter"),
      vapply(candidates,stored_value,character(1),"DenBefore"),
      vapply(candidates,stored_value,character(1),"DenAfter"),
      vapply(candidates,function(r)ifelse(is.na(r$NumPos),.Machine$integer.max,r$NumPos),integer(1)),
      vapply(candidates,function(r)ifelse(is.na(r$DenPos),.Machine$integer.max,r$DenPos),integer(1)),
      vapply(candidates,function(r)isTRUE(r$Invert),logical(1)),seq_along(candidates)),
      list(na.last=TRUE,method="radix")))
    rank<-integer(length(ord));rank[ord]<-seq_along(ord)
    for(i in seq_along(evaluated)){evaluated[[i]]$CandidateRank<-rank[i];evaluated[[i]]$Selected<-FALSE}
    replay_candidate<-function(i){
      original<-candidates[[i]]
      normalized<-tryCatch(coerce_contract(list(table=empty_content(),condition=empty_condition(),ratio=original))$ratio,error=function(e)e)
      if(inherits(normalized,"error"))return(list(ok=FALSE,reason=paste("method-specific fields were changed during coercion:",conditionMessage(normalized))))
      fields<-setdiff(RATIO_FIELDS,"Content")
      changed<-fields[!vapply(fields,function(n)identical(original[[n]],normalized[[n]]),logical(1))]
      if(length(changed))return(list(ok=FALSE,reason=sprintf("method-specific fields were changed during coercion: %s",paste(changed,collapse=", "))))
      replay_rule<-unserialize(serialize(normalized,NULL,version=2L))
      # Stage 1: replay the normalized, serialized candidate on only the
      # reference rows for this Content.  This is the sole acceptance gate.
      direct_metadata<-df[idx,,drop=FALSE]
      direct_metadata$Content<-rep(label,nrow(direct_metadata))
      direct<-tryCatch(apply_ratio_table(direct_metadata,replay_rule,ns,ds),error=function(e)e)
      if(inherits(direct,"error"))return(list(ok=FALSE,reason=paste("Ratio rule replay failed:",conditionMessage(direct))))
      d<-direct$diagnostics
      bad_n<-which(d$PredictedNumerator!=d$ExpectedNumerator)
      bad_d<-which(d$PredictedDenominator!=d$ExpectedDenominator)
      if(length(bad_n)){
        return(list(ok=FALSE,reason="Ratio rule replay failed",diagnostics=d,targeted=rep(TRUE,nrow(d))))
      }
      if(length(bad_d)){
        return(list(ok=FALSE,reason="Ratio rule replay failed",diagnostics=d,targeted=rep(TRUE,nrow(d))))
      }
      if(!all(d$Success))return(list(ok=FALSE,reason="Ratio rule replay failed",diagnostics=d,targeted=rep(TRUE,nrow(d))))

      # Stage 2: diagnose the whole pipeline without changing acceptance.
      pipeline<-tryCatch(apply_ratio_table(pipeline_metadata,replay_rule,chr(df$Numerator),chr(df$Denominator)),error=function(e)e)
      targeted<-chr(pipeline_metadata$Content[idx])==label
      warning<-""
      reachability<-if(all(targeted))"reached" else "not_reached"
      content_regex<-""
      reachability_reason<-"Content assignment targeted every applicable reference row."
      if(any(!targeted)) {
        selected_content<-content_rules[content_rules$Content==label,,drop=FALSE]
        if(nrow(selected_content)) {
          selected_content<-selected_content[1L,,drop=FALSE]
          content_regex<-selected_content$Include
          missed_sources<-chr(df$Column[idx])[!targeted]
          include_miss<-!safe_grepl(selected_content$Include,missed_sources)
          excluded<-if(nzchar(selected_content$Exclude))safe_grepl(selected_content$Exclude,missed_sources)else rep(FALSE,length(missed_sources))
          detail<-if(any(include_miss))"the include regex did not match" else if(any(excluded))"the exclude regex matched" else "a later content rule reassigned the row"
          reachability_reason<-sprintf("Selected content regex '%s' missed source '%s': %s.",
            selected_content$Include,paste(missed_sources,collapse=" | "),detail)
        } else {
          reachability_reason<-sprintf("No content rule was selected for '%s'; source '%s' was therefore not assigned.",
            label,paste(chr(df$Column[idx])[!targeted],collapse=" | "))
        }
        warning<-paste("Ratio rule valid;",reachability_reason)
      } else if(original$Method=="Pattern Recognition" &&
          (inherits(pipeline,"error") || !all(pipeline$diagnostics$Success[idx]))) {
        warning<-"Ratio rule valid; Pattern Recognition context was unavailable after condition assignment"
      }
      list(ok=TRUE,reason="",rule=replay_rule,diagnostics=d,targeted=targeted,warning=warning,
        reachability=reachability,content_regex=content_regex,reachability_reason=reachability_reason)
    }
    complete_order<-ord[complete[ord]];best<-if(length(complete_order))complete_order[1L]else ord[1L]
    selected<-NA_integer_;gate_reason<-"";replay_failures<-character();pipeline_warning<-""
    pipeline_reachability<-"not_evaluated";selected_content_regex<-"";pipeline_reason<-"Ratio rule was not accepted for pipeline replay."
    for(i in complete_order){
      replay_result<-replay_candidate(i)
      if(!is.null(replay_result$diagnostics)){
        evaluated[[i]]$ReplayNumerator<-replay_result$diagnostics$PredictedNumerator
        evaluated[[i]]$ReplayDenominator<-replay_result$diagnostics$PredictedDenominator
        evaluated[[i]]$ReplaySuccess<-replay_result$diagnostics$Success
        evaluated[[i]]$ApplicableContentTargeted<-replay_result$targeted
      }
      evaluated[[i]]$ReplayFailureReason<-if(isTRUE(replay_result$ok))"" else replay_result$reason
      if(isTRUE(replay_result$ok)){selected<-i;candidates[[i]]<-replay_result$rule
        pipeline_reachability<-replay_result$reachability
        selected_content_regex<-replay_result$content_regex
        pipeline_reason<-replay_result$reachability_reason
        if(nzchar(replay_result$warning)){pipeline_warning<-replay_result$warning
          warnings<-c(warnings,sprintf("%s: %s.",label,pipeline_warning))}
        break}
      replay_failures<-c(replay_failures,sprintf("candidate %d: %s",i,replay_result$reason))
      evaluated[[i]]$FailureReason[!nzchar(evaluated[[i]]$FailureReason)]<-replay_result$reason
    }
    reliable<-!is.na(selected)
    if(reliable)best<-selected else if(length(replay_failures))gate_reason<-paste(replay_failures,collapse="; ")
    status<-if(reliable)"reliable"else"unresolved"
    logger("trace","ratio","score",sprintf("%s tie-break: success=%d/%d, method rank=%d, complexity=%d, candidate rank=%d.",
      label,scores[best],length(rows),simplicity[best],complexity[best],rank[best]))
    logger("debug","ratio","select",sprintf("%s: %s method '%s'; reason=%s; elapsed=%.1f ms.",label,status,candidates[[best]]$Method,
      if(reliable)"all applicable components round-tripped" else if(nzchar(gate_reason))gate_reason else "candidate did not reproduce every applicable row",
      (proc.time()[["elapsed"]]-label_started)*1000),details=list(selected_method=if(reliable)candidates[[best]]$Method else "",elapsed_ms=(proc.time()[["elapsed"]]-label_started)*1000))
    if(reliable&&length(rows)==1L)logger("trace","ratio","evidence","Neutral diagnostic: exact ratio rule was validated against one applicable row.")
    if(reliable)evaluated[[best]]$Selected<-TRUE;diag_blocks<-c(diag_blocks,evaluated)
    if(reliable)out<-rbind(out,candidates[[best]])else warnings<-c(warnings,if(nzchar(gate_reason))sprintf("%s ratio extraction unresolved: %s.",label,gate_reason)else sprintf("%s ratio extraction %s: best candidate reproduced %d/%d applicable rows.",label,status,scores[best],length(rows)))
    statuses<-rbind(statuses,data.frame(Content=label,SelectedMethod=if(reliable)candidates[[best]]$Method else "",
      Status=status,RatioRuleStatus=status,PipelineReachabilityStatus=pipeline_reachability,
      SelectedContentRegex=selected_content_regex,PipelineReachabilityReason=pipeline_reason,
      SuccessfulRows=scores[best],ApplicableRows=length(rows),
      Reason=if(reliable)pipeline_warning else tail(warnings,1L),stringsAsFactors=FALSE))
  }
  diag<-if(length(diag_blocks))do.call(rbind,diag_blocks)else data.frame()
  if(nrow(diag))diag<-diag[order(diag$Content,diag$CandidateRank,diag$Row),,drop=FALSE]
  logger("info","ratio","infer",sprintf("Ratio inference completed: %d rules and %d non-reliable labels.",nrow(out),sum(statuses$Status!="reliable")),
    details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  list(table=out,status=statuses,diagnostics=diag,warnings=unique(warnings))
}

# ---- application, diagnostics, compatibility validation --------------------
apply_content_table <- function(metadata, table) {
  stopifnot(is.data.frame(metadata), identical(names(table), CONTENT_FIELDS), "Column" %in% names(metadata))
  out <- metadata
  expected <- if ("Content" %in% names(out)) chr(out$Content) else rep("", nrow(out))
  expected_transformation <- if ("Transformation" %in% names(out))
    normalize_transformation_values(expected,out$Transformation) else vapply(expected,function(label)
      unname(infer_content_transformation(metadata,label)),character(1))
  out$Content <- rep("", nrow(out)); out$Transformation <- rep(NA_character_, nrow(out))
  hits <- matrix(FALSE, nrow(out), nrow(table))
  for (i in seq_len(nrow(table))) {
    hit <- safe_grepl(table$Include[i], out$Column)
    if (!is.na(table$Exclude[i]) && nzchar(table$Exclude[i])) hit <- hit & !safe_grepl(table$Exclude[i], out$Column)
    hits[, i] <- hit
    out$Content[hit] <- table$Content[i]
  }
  # Match Data Wizard replay exactly: after content assignment, each rule's
  # transformation is applied to every row with that final Content.  Therefore
  # duplicate rules for one Content would overwrite the whole content class.
  for (i in seq_len(nrow(table)))
    out$Transformation[chr(out$Content) == table$Content[i]] <- table$Transformation[i]
  counts <- rowSums(hits)
  transformation_match <- mapply(function(expected,predicted)
    (is.na(expected)&&is.na(predicted)) || (!is.na(expected)&&!is.na(predicted)&&identical(expected,predicted)),
    expected_transformation,out$Transformation,USE.NAMES=FALSE)
  mismatch_reason <- ifelse(transformation_match,"",
    ifelse(is.na(expected_transformation),"Expected NA for non-transformable content.",
      ifelse(is.na(out$Transformation),"Predicted transformation is unresolved or not assigned.",
        sprintf("Expected '%s' but predicted '%s'.",expected_transformation,out$Transformation))))
  row_diagnostics <- data.frame(Row=seq_len(nrow(out)), Column=chr(out$Column), Expected=expected,
    Predicted=chr(out$Content), Match=chr(out$Content)==expected,
    ExpectedTransformation=expected_transformation,PredictedTransformation=out$Transformation,
    TransformationMatch=transformation_match,TransformationFailureReason=mismatch_reason,
    Unresolved=!nzchar(chr(out$Content)), Conflict=counts>1L, MatchingRules=counts, stringsAsFactors=FALSE)
  metrics <- if (!nrow(table)) data.frame() else do.call(rbind, lapply(seq_len(nrow(table)), function(i) {
    truth <- expected == table$Content[i]; hit <- hits[, i]
    tp<-sum(hit&truth);fp<-sum(hit&!truth);fn<-sum(!hit&truth);precision<-if(tp+fp)tp/(tp+fp)else 0;recall<-if(tp+fn)tp/(tp+fn)else 0
    data.frame(Content=table$Content[i],Include=table$Include[i],Exclude=table$Exclude[i],FalsePositives=fp,FalseNegatives=fn,Precision=precision,Recall=recall,F1=if(precision+recall)2*precision*recall/(precision+recall)else 0,Coverage=mean(hit),stringsAsFactors=FALSE)
  }))
  list(metadata=out, metrics=metrics, rows=row_diagnostics,
       conflicts=row_diagnostics[row_diagnostics$Conflict,,drop=FALSE])
}

# Summarize the final, ordered application result rather than adding together
# per-rule scores (which double count overlaps and omit classes without rules).
content_assignment_summary <- function(rows, table) {
  stopifnot(is.data.frame(rows), all(c("Expected","Predicted","Match","Unresolved","Conflict") %in% names(rows)))
  expected <- chr(rows$Expected); predicted <- chr(rows$Predicted)
  mismatch <- predicted != expected
  expected_labels <- unique(expected[nzchar(trimws(expected))])
  selected_labels <- unique(chr(table$Content))
  no_rule <- setdiff(expected_labels, selected_labels)
  list(
    assigned_correctly=sum(nzchar(predicted) & !mismatch),
    false_positives=sum(nzchar(predicted) & mismatch),
    false_negatives=sum(nzchar(expected) & mismatch),
    unresolved_rows=sum(!nzchar(predicted)),
    conflicts=sum(rows$Conflict),
    labels_with_no_selected_rule=length(no_rule),
    labels_without_selected_rule=no_rule
  )
}

apply_condition_table <- function(metadata, table, expected=character()) {
  stopifnot(is.data.frame(metadata), identical(names(table), CONDITION_FIELDS))
  out <- metadata
  # Assignment rules overwrite rows they target; they do not erase an existing
  # verified assignment on unrelated content rows.
  if(!"Options"%in%names(out))out$Options <- rep("", nrow(out))
  else out$Options <- chr(out$Options)
  # Invalid assistant condition rows are ignored, and their explicitly observed
  # non-sample metadata rows cannot contribute stale values to sample lookup.
  non_sample_rows <- !is_sample_bearing_content(out$Content)
  out$Options[non_sample_rows] <- ""
  # Ratio Options is repository content-assignment state, not an inferred
  # sample.  Restore it independently of condition-rule application.
  ratio_rows <- chr(out$Content) %in% AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT
  out$Options[ratio_rows] <- "Ratio"
  table <- table[is_sample_bearing_content(table$Content),,drop=FALSE]
  failures <- list(); targeted <- rep(FALSE,nrow(out))
  for (i in seq_len(nrow(table))) {
    rows <- which(chr(out$Content) == table$Content[i])
    if (!length(rows)) next
    targeted[rows] <- TRUE
    value <- extract_condition(chr(out$Column[rows]), table$Method[i], table$Before[i], table$After[i], table$Separators[i], table$Pos[i])
    extracted <- !is.na(value) & nzchar(value)
    out$Options[rows[extracted]] <- value[extracted]
    bad <- is.na(value) | !nzchar(value)
    if (any(bad)) failures[[length(failures)+1L]] <- data.frame(Rule=i, Content=table$Content[i], Row=rows[bad], Column=chr(out$Column[rows[bad]]), FailureReason="Condition was not extracted")
  }
  expected <- if (length(expected)==nrow(out)) chr(expected) else rep("",nrow(out))
  diagnostics <- data.frame(Row=seq_len(nrow(out)),Column=chr(out$Column),Content=chr(out$Content),PredictedCondition=chr(out$Options),ExpectedCondition=expected,ExtractionFailure=targeted&!nzchar(chr(out$Options)),ExactMatch=if(length(expected))chr(out$Options)==expected else NA,stringsAsFactors=FALSE)
  list(metadata=out, diagnostics=diagnostics, failures=if(length(failures))do.call(rbind,failures)else data.frame())
}

apply_ratio_table <- function(metadata, table, expected_numerator=character(), expected_denominator=character()) {
  stopifnot(is.data.frame(metadata), identical(names(table), RATIO_FIELDS))
  out<-metadata;out$Numerator<-out$Denominator<-rep("",nrow(out)); targeted<-rep(FALSE,nrow(out))
  known<-known_samples_after_conditions(out)
  for(i in seq_len(nrow(table))){rows<-which(chr(out$Content)==table$Content[i]);if(!length(rows))next;targeted[rows]<-TRUE
    got<-lapply(chr(out$Column[rows]),ratio_extract,rule=table[i,,drop=FALSE],known=known)
    for(j in seq_along(rows))if(!is.null(got[[j]])){out$Numerator[rows[j]]<-ifelse(is.na(got[[j]]$numerator),"",got[[j]]$numerator);out$Denominator[rows[j]]<-ifelse(is.na(got[[j]]$denominator),"",got[[j]]$denominator)}
  }
  en<-if(length(expected_numerator)==nrow(out))chr(expected_numerator)else rep("",nrow(out));ed<-if(length(expected_denominator)==nrow(out))chr(expected_denominator)else rep("",nrow(out))
  failure<-targeted&(!nzchar(chr(out$Numerator))|!nzchar(chr(out$Denominator)))
  diagnostics<-data.frame(Row=seq_len(nrow(out)),Column=chr(out$Column),Content=chr(out$Content),PredictedNumerator=chr(out$Numerator),ExpectedNumerator=en,PredictedDenominator=chr(out$Denominator),ExpectedDenominator=ed,Success=nzchar(chr(out$Numerator))&nzchar(chr(out$Denominator))&out$Numerator==en&out$Denominator==ed,ExtractionFailure=failure,FailureReason=ifelse(failure,"Ratio components were not fully extracted",ifelse(out$Numerator!=en,"Numerator mismatch",ifelse(out$Denominator!=ed,"Denominator mismatch",""))),stringsAsFactors=FALSE)
  list(metadata=out,diagnostics=diagnostics,failures=diagnostics[diagnostics$ExtractionFailure,,drop=FALSE])
}

test_rules <- function(df,rules) {
  pred<-rep("",nrow(df)); conflicts<-integer(nrow(df)); for(i in seq_len(nrow(rules$table))){hit<-safe_grepl(rules$table$Include[i],df$Column);if(!is.na(rules$table$Exclude[i])&&nzchar(rules$table$Exclude[i]))hit<-hit&!safe_grepl(rules$table$Exclude[i],df$Column);pred[hit]<-rules$table$Content[i];conflicts[hit]<-conflicts[hit]+1L}
  data.frame(Row=seq_len(nrow(df)),Column=chr(df$Column),Expected=if("Content"%in%names(df))chr(df$Content)else"",Predicted=pred,Match=if("Content"%in%names(df))pred==chr(df$Content)else NA,Conflict=conflicts>1)
}
coerce_contract <- function(rules) {
  # Coerce storage modes without globally replacing NA.  In particular, ratio
  # method-specific NAs are part of the persisted contract, not missing input.
  rules$table<-rules$table[,CONTENT_FIELDS,drop=FALSE]; rules$condition<-rules$condition[,CONDITION_FIELDS,drop=FALSE];rules$ratio<-rules$ratio[,RATIO_FIELDS,drop=FALSE]
  for(n in CONTENT_FIELDS) rules$table[[n]]<-as.character(rules$table[[n]])
  rules$table$Transformation <- normalize_transformation_values(
    rules$table$Content,rules$table$Transformation)
  for(n in CONDITION_FIELDS[1:5]) rules$condition[[n]]<-as.character(rules$condition[[n]]);rules$condition$Pos<-as.integer(rules$condition$Pos)
  for(n in c("Content","Method","Separators","NumBefore","NumAfter","DenBefore","DenAfter"))rules$ratio[[n]]<-as.character(rules$ratio[[n]]);rules$ratio$Invert<-as.logical(rules$ratio$Invert);rules$ratio$NumPos<-as.integer(rules$ratio$NumPos);rules$ratio$DenPos<-as.integer(rules$ratio$DenPos)
  # UI-created condition rows use empty character values and retain Pos=1 even
  # where Pos is ignored; normalize imported spreadsheet blanks accordingly.
  for(n in c("Before","After","Separators")) rules$condition[[n]][is.na(rules$condition[[n]])] <- ""
  rules$condition$Pos[is.na(rules$condition$Pos) & !rules$condition$Method %in% c("phrase_position","pattern_detect")] <- 1L
  rules
}
data_wizard_normalize_rules <- function(rules) {
  # Both Data Wizard loaders consume these three named data frames directly.
  if (!is.list(rules) || !all(c("table", "condition", "ratio") %in% names(rules)))
    stop("Data Wizard requires table, condition, and ratio components.")
  out <- rules[c("table", "condition", "ratio")]
  if (!all(vapply(out, is.data.frame, logical(1)))) stop("Every rule component must be a data frame.")
  out$table <- out$table[, CONTENT_FIELDS, drop=FALSE]
  out$condition <- out$condition[, CONDITION_FIELDS, drop=FALSE]
  out$ratio <- out$ratio[, RATIO_FIELDS, drop=FALSE]
  coerce_contract(out)
}

build_export_template <- function(rules, exported_at=Sys.time(), logger=.miraprot_noop_logger,
                                  rules_are_normalized=FALSE) {
  started<-proc.time()[["elapsed"]]
  core <- if (isTRUE(rules_are_normalized)) rules[c("table","condition","ratio")] else data_wizard_normalize_rules(rules)
  result<-c(core, list(debug_info=list(
    exported_at=as.POSIXct(exported_at),
    export_options=list(save_ui=FALSE, include_filtering=FALSE,
      include_imputation=FALSE, include_batch_effects=FALSE,
      include_pivot=FALSE, include_merge=FALSE, include_edit_ops=FALSE,
      include_ratios=FALSE),
    components_exported="assignment_rules",
    module_versions=list(auto_assign="modular_v1", filtering="enhanced_v2",
      imputation="enhanced_v1", edit_operations="enhanced_v1",
      batch_effects="enhanced_v1", pivot="enhanced_v1", merge="enhanced_v1"),
    last_import_info=NULL, debug_level=0, processing_history=list()
  )))
  logger("info","export","construct",sprintf("RDS object constructed with %d content, %d condition, and %d ratio rules.",
    nrow(core$table),nrow(core$condition),nrow(core$ratio)),details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  result
}

regex_complexity <- function(x) {
  x <- chr(x)
  # Bound constructs which cause branching/backtracking, rather than merely
  # counting harmless literal characters.
  lengths(regmatches(x, gregexpr("[|*+?{}()]|\\(\\?", x, perl=TRUE)))
}

validate_export <- function(rules, metadata=NULL, logger=.miraprot_noop_logger) {
  started<-proc.time()[["elapsed"]]
  errors <- character(); add <- function(x) errors <<- c(errors, x)
  if (!is.list(rules) || !all(c("table","condition","ratio") %in% names(rules)))
    return("Rule collection must contain table, condition, and ratio components.")
  if (!identical(names(rules)[seq_len(3L)], c("table","condition","ratio")))
    add("table, condition, and ratio must be the first components in that order.")
  core <- rules[c("table","condition","ratio")]
  schemas <- list(table=CONTENT_FIELDS, condition=CONDITION_FIELDS, ratio=RATIO_FIELDS)
  classes <- list(table=rep("character",4), condition=c(rep("character",5),"integer"),
                  ratio=c("character","character","character","logical",rep("character",4),"integer","integer"))
  for (component in names(schemas)) {
    x <- core[[component]]
    if (!is.data.frame(x)) { add(sprintf("%s must be a data frame.", component)); next }
    if (!identical(names(x), schemas[[component]])) { add(sprintf("%s fields or field order are invalid.", component)); next }
    actual <- vapply(x, function(z) class(z)[1L], character(1))
    if (!identical(unname(actual), classes[[component]])) add(sprintf("%s column classes are invalid (expected %s).", component, paste(classes[[component]],collapse=", ")))
  }
  if (length(errors)) return(unique(errors))
  core$table$Transformation <- normalize_transformation_values(
    core$table$Content,core$table$Transformation)
  blank <- function(x) is.na(x) | !nzchar(trimws(x))
  if (any(blank(core$table$Content)) || any(blank(core$table$Include))) add("Content rules require nonblank Content and Include values.")
  if (any(blank(core$condition$Content)) || any(blank(core$condition$Method))) add("Condition rules require nonblank Content and Method values.")
  for (message in condition_content_validation_messages(core$condition)) add(message)
  if (any(blank(core$ratio$Content)) || any(blank(core$ratio$Method)) || any(is.na(core$ratio$Invert))) add("Ratio rules require Content, Method, and Invert values.")
  if (any(!core$condition$Method %in% CONDITION_METHODS)) add("Unsupported condition method.")
  if (any(!core$ratio$Method %in% RATIO_METHODS)) add("Unsupported ratio method.")
  row_index <- core$table$Content == "Row Index"
  if (sum(row_index, na.rm=TRUE) != 1L || !identical(core$table$Include[row_index], "Row Index") ||
      !identical(core$table$Exclude[row_index], "") || !is.na(core$table$Transformation[row_index]))
    add("Content rules require exactly one canonical Row Index special row.")
  transformable <- core$table$Content %in% TRANSFORMATION_CONTENT_TYPES
  invalid_transform <- transformable & (blank(core$table$Transformation) |
    !core$table$Transformation %in% SUPPORTED_TRANSFORMATIONS)
  if (any(invalid_transform)) for (i in which(invalid_transform)) add(sprintf(
    "Content rule '%s' has an invalid or ambiguous Transformation; choose exactly one of %s before export.",
    core$table$Content[i],paste(SUPPORTED_TRANSFORMATIONS,collapse=", ")))
  invalid_nontransform <- !transformable & !is.na(core$table$Transformation)
  if (any(invalid_nontransform)) for(i in which(invalid_nontransform)) add(sprintf(
    "Content rule '%s' does not support transformations; set Transformation to NA.",core$table$Content[i]))
  duplicated_content <- unique(core$table$Content[duplicated(core$table$Content) |
    duplicated(core$table$Content,fromLast=TRUE)])
  for(label in duplicated_content) {
    values<-unique(core$table$Transformation[core$table$Content==label])
    values_key<-ifelse(is.na(values),"<NA>",values)
    if(length(unique(values_key))>1L)add(sprintf(
      "Content '%s' has conflicting Transformation values across multiple rules; retain one effective rule or choose one transformation.",label))
  }
  if ("debug_info" %in% names(rules)) {
    info <- rules$debug_info
    required_info <- c("exported_at","export_options","components_exported","module_versions",
      "last_import_info","debug_level","processing_history")
    if (!is.list(info) || !all(required_info %in% names(info))) add("debug_info is missing required diagnostic fields.")
    else {
      flags <- c("save_ui","include_filtering","include_imputation","include_batch_effects",
        "include_pivot","include_merge","include_edit_ops","include_ratios")
      if (!inherits(info$exported_at,"POSIXct")) add("debug_info$exported_at must be POSIXct/POSIXt.")
      if (!is.list(info$export_options) || !all(flags %in% names(info$export_options)) ||
          any(!vapply(info$export_options[flags], function(x)is.logical(x)&&length(x)==1L&&!is.na(x), logical(1))))
        add("debug_info$export_options must contain the eight scalar logical flags.")
      if (!is.character(info$components_exported) || !"assignment_rules" %in% info$components_exported) add("debug_info$components_exported must include assignment_rules.")
      if (!is.list(info$module_versions) || !is.list(info$processing_history) ||
          !is.numeric(info$debug_level) || length(info$debug_level)!=1L) add("debug_info diagnostic field classes are invalid.")
    }
  }
  cnd <- core$condition
  if (any(cnd$Method=="between" & (blank(cnd$Before)|blank(cnd$After)))) add("Condition method 'between' requires Before and After.")
  if (any(cnd$Method=="start" & blank(cnd$After))) add("Condition method 'start' requires After.")
  if (any(cnd$Method=="end" & blank(cnd$Before))) add("Condition method 'end' requires Before.")
  positional <- cnd$Method %in% c("phrase_position","pattern_detect")
  if (any(positional & (blank(cnd$Separators)|is.na(cnd$Pos)|cnd$Pos<1L))) add("Positional condition methods require Separators and a positive Pos.")
  rat <- core$ratio
  if (any(rat$Method=="Position in String" & (blank(rat$Separators)|is.na(rat$NumPos)|rat$NumPos<1L|is.na(rat$DenPos)|rat$DenPos<1L))) add("Position in String ratio rules require Separators and positive numerator/denominator positions.")
  if (any(rat$Method=="Pattern Recognition" & blank(rat$Separators))) add("Pattern Recognition ratio rules require Separators.")
  re_rows <- rat$Method=="Regular Expressions"
  if (any(re_rows & blank(rat$NumAfter) & blank(rat$DenBefore))) add("Regular Expressions ratio rules require NumAfter or DenBefore.")
  # Enforce the exact method-dependent empty representation emitted by the UI.
  if (any(rat$Method=="Regular Expressions" & (!is.na(rat$Separators)|!is.na(rat$NumPos)|!is.na(rat$DenPos)|rat$Invert))) add("Regular Expressions ratio rows require NA separators/positions and Invert FALSE.")
  non_regex <- rat$Method %in% c("Pattern Recognition","Position in String")
  if (any(non_regex & (!is.na(rat$NumBefore)|!is.na(rat$NumAfter)|!is.na(rat$DenBefore)|!is.na(rat$DenAfter)))) add("Non-regex ratio rows require NA regex boundaries.")
  if (any(rat$Method=="Pattern Recognition" & (!is.na(rat$NumPos)|!is.na(rat$DenPos)))) add("Pattern Recognition ratio rows require NA positions.")
  patterns <- c(core$table$Include,core$table$Exclude,cnd$Before,cnd$After,cnd$Separators,rat$Separators,rat$NumBefore,rat$NumAfter,rat$DenBefore,rat$DenAfter)
  patterns <- chr(patterns); used <- nzchar(patterns)
  if (any(nchar(patterns[used], type="chars") > MAX_REGEX_LENGTH)) add(sprintf("Regex values may not exceed %d characters.", MAX_REGEX_LENGTH))
  if (any(regex_complexity(patterns[used]) > MAX_REGEX_COMPLEXITY)) add(sprintf("Regex values may not exceed %d complexity constructs.", MAX_REGEX_COMPLEXITY))
  pcre_results <- lapply(patterns[used], validate_pcre)
  if (any(!vapply(pcre_results, `[[`, logical(1), "valid"))) add("One or more regex values are invalid for PCRE.")
  stringr_patterns <- chr(c(cnd$Separators[positional], rat$Separators[non_regex]))
  stringr_patterns <- stringr_patterns[nzchar(stringr_patterns)]
  stringr_results <- lapply(stringr_patterns, validate_stringr_pattern)
  if (any(!vapply(stringr_results, `[[`, logical(1), "valid"))) add("One or more separator values are invalid for stringr/ICU.")
  logger("debug","regex","validate",sprintf("Regex validation checked %d PCRE and %d stringr patterns: %d invalid.",
    length(pcre_results),length(stringr_results),sum(!vapply(c(pcre_results,stringr_results),`[[`,logical(1),"valid"))))
  if (!is.null(metadata) && !length(errors)) {
    application_error <- tryCatch({
      a <- apply_content_table(metadata, core$table)
      b <- apply_condition_table(a$metadata, core$condition)
      apply_ratio_table(b$metadata, core$ratio); NULL
    }, error=function(e) conditionMessage(e))
    if (!is.null(application_error)) add(paste("Rules cannot be applied to current metadata:", application_error))
  }
  result<-unique(errors)
  logger(if(length(result))"warning" else "info","export","validate",sprintf("Export validation completed with %d error(s).",length(result)),
    details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  result
}

roundtrip_export <- function(rules, metadata, logger=.miraprot_noop_logger) {
  started<-proc.time()[["elapsed"]]
  path <- tempfile(fileext=".rds"); on.exit(unlink(path), add=TRUE)
  template <- build_export_template(rules, logger=logger)
  saveRDS(template, path); reloaded <- readRDS(path)
  if (!identical(template, reloaded) || !identical(lapply(template, class), lapply(reloaded, class))) stop("RDS round trip changed object structure or classes.")
  normalized <- data_wizard_normalize_rules(reloaded)
  problems <- validate_export(reloaded, metadata, logger)
  if (length(problems)) stop(paste(problems, collapse=" "))
  logger("info","export","roundtrip",sprintf("RDS round trip preserved all components and applied %d metadata row(s) successfully.",nrow(metadata)),
    details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  reloaded
}

signature_hash <- function(payload) {
  path <- tempfile(); on.exit(unlink(path), add=TRUE)
  writeBin(serialize(payload,NULL,version=2L),path)
  unname(tools::md5sum(path))
}

# Pure policy boundary for the Export tab.  Status values, rather than warning
# prose, are the sole authority for unreliable-rule selection.  Row Index is a
# mandatory compatibility rule and therefore cannot be removed by policy.
prepare_export_inputs <- function(rules, metrics, metadata, unreliable_action, unresolved_action) {
  stopifnot(unreliable_action %in% c("exclude","include"),
    unresolved_action %in% c("exclude","include"))
  if (is.null(rules) || !is.list(rules)) stop("No inferred rules exist.")
  if (is.null(metadata) || !is.data.frame(metadata)) stop("Mapped metadata do not exist.")
  core <- rules[c("table","condition","ratio")]
  status_sets <- if (is.list(metrics) && !is.null(metrics$rule_status)) metrics$rule_status else metrics
  unreliable <- lapply(c("table","condition","ratio"), function(component) {
    diagnostic <- if (is.list(status_sets)) status_sets[[component]] else NULL
    if (is.null(diagnostic) && component == "table" && is.list(status_sets)) diagnostic <- status_sets$content
    if (!is.data.frame(diagnostic) || !"Content" %in% names(diagnostic)) return(character())
    bad <- if ("Reliable" %in% names(diagnostic)) !diagnostic$Reliable else
      if ("Status" %in% names(diagnostic)) tolower(chr(diagnostic$Status)) %in% c("unresolved","provisional","conflicting") else rep(FALSE,nrow(diagnostic))
    unique(chr(diagnostic$Content[!is.na(bad) & bad]))
  }); names(unreliable) <- c("table","condition","ratio")
  effective <- core
  excluded_rules <- integer(3L); names(excluded_rules) <- names(effective)
  invalid_condition_rows <- !is_sample_bearing_content(effective$condition$Content)
  excluded_rules[["condition"]] <- sum(invalid_condition_rows)
  effective$condition <- effective$condition[!invalid_condition_rows,,drop=FALSE]
  if (identical(unreliable_action,"exclude")) for (component in names(effective)) {
    remove <- effective[[component]]$Content %in% unreliable[[component]]
    if (component == "table") remove <- remove & effective[[component]]$Content != "Row Index"
    excluded_rules[[component]] <- excluded_rules[[component]] + sum(remove)
    effective[[component]] <- effective[[component]][!remove,,drop=FALSE]
  }
  included_rules <- vapply(effective,nrow,integer(1))
  included_unreliable <- vapply(names(effective),function(component)
    sum(effective[[component]]$Content %in% unreliable[[component]]),integer(1))

  # Prefer retained retest row diagnostics when supplied.  Otherwise create the
  # same structured diagnostic with the selected content rules; never create a
  # rule for an unresolved row.
  rows <- if (is.list(metrics) && is.data.frame(metrics$rows)) metrics$rows else NULL
  if (is.null(rows) || !"Unresolved" %in% names(rows) || nrow(rows) != nrow(metadata))
    rows <- apply_content_table(metadata,effective$table)$rows
  unresolved <- !is.na(rows$Unresolved) & rows$Unresolved
  keep <- if (identical(unresolved_action,"exclude")) !unresolved else rep(TRUE,nrow(metadata))
  effective_metadata <- metadata[keep,,drop=FALSE]
  replay_rows <- rows[keep,,drop=FALSE]
  counts <- list(rules=list(included=included_rules,excluded_unreliable=excluded_rules,
      included_unreliable=included_unreliable),
    metadata=list(included=sum(keep),excluded_unresolved=sum(!keep),
      included_unresolved=sum(unresolved & keep),unresolved=sum(unresolved)))
  warnings <- character()
  invalid_types <- invalid_condition_content(core$condition)
  if (length(invalid_types)) warnings <- c(warnings,sprintf(
    "Excluded non-sample condition rule Content '%s'; change its Content type to include it.",invalid_types))
  if (sum(excluded_rules)) warnings <- c(warnings,sprintf("Excluded %d unreliable rule row(s).",sum(excluded_rules)))
  if (sum(included_unreliable)) warnings <- c(warnings,sprintf("Included %d diagnostically unreliable rule row(s).",sum(included_unreliable)))
  if (sum(!keep)) warnings <- c(warnings,sprintf("Excluded %d unresolved metadata row(s) from replay diagnostics.",sum(!keep)))
  if (sum(unresolved & keep)) warnings <- c(warnings,sprintf("Included %d unresolved metadata row(s) in replay diagnostics.",sum(unresolved & keep)))
  payload <- list(rules=effective,metadata=effective_metadata,replay_rows=replay_rows,
    actions=list(unreliable_action=unreliable_action,unresolved_action=unresolved_action),counts=counts)
  list(table=effective$table,condition=effective$condition,ratio=effective$ratio,
    metadata=effective_metadata,replay_rows=replay_rows,counts=counts,warnings=warnings,
    signature_payload=payload)
}

export_signature <- function(rules, metadata, inclusion, metrics=NULL) {
  prepared <- prepare_export_inputs(rules,metrics,metadata,inclusion$unreliable_action,inclusion$unresolved_action)
  signature_hash(prepared$signature_payload)
}

compare_export_objects <- function(expected, actual, values=TRUE) {
  if (!identical(names(expected),names(actual))) return("RDS round trip changed top-level components.")
  for (component in c("table","condition","ratio")) {
    if (!is.data.frame(expected[[component]]) || !is.data.frame(actual[[component]]))
      return(sprintf("RDS round trip changed the %s table type.",component))
    if (!identical(names(expected[[component]]),names(actual[[component]])))
      return(sprintf("RDS round trip changed the %s schema.",component))
    expected_classes<-vapply(expected[[component]],function(x)class(x)[1L],character(1))
    actual_classes<-vapply(actual[[component]],function(x)class(x)[1L],character(1))
    if (!identical(expected_classes,actual_classes)) return(sprintf("RDS round trip changed the %s column classes.",component))
    if (isTRUE(values) && !identical(expected[[component]],actual[[component]]))
      return(sprintf("RDS round trip changed the %s values.",component))
  }
  if (isTRUE(values) && !identical(expected,actual)) return("RDS round trip changed object values.")
  character()
}

prepare_export_artifact <- function(rules, metadata, inclusion,
  logger=.miraprot_noop_logger, operations=list(normalize=coerce_contract,
    validate=validate_export, construct=build_export_template,
    save=saveRDS, read=readRDS), metrics=NULL) {
  started<-proc.time()[["elapsed"]]
  logger("info","export","roundtrip","Validated export preparation started.")
  result<-tryCatch({
    if (is.null(rules)) stop("No inferred rules exist.")
    if (is.null(metadata)) stop("Mapped metadata do not exist.")
    selected<-prepare_export_inputs(rules,metrics,metadata,inclusion$unreliable_action,inclusion$unresolved_action)
    # Selection deliberately precedes schema coercion and validation.
    normalized<-operations$normalize(selected[c("table","condition","ratio")])
    effective_metadata<-selected$metadata
    problems<-operations$validate(normalized,logger=.miraprot_noop_logger)
    if(length(problems))stop(paste(problems,collapse=" "))
    template<-operations$construct(normalized,logger=.miraprot_noop_logger,rules_are_normalized=TRUE)
    path<-tempfile(fileext=".rds");on.exit(unlink(path),add=TRUE)
    operations$save(template,path);reloaded<-operations$read(path)
    changed<-compare_export_objects(template,reloaded,values=TRUE)
    if(length(changed))stop(changed)
    # Exercise the effective loader and replay each rule family against the
    # metadata remaining after the unresolved-row choice is applied.
    loaded<-data_wizard_normalize_rules(reloaded)
    content<-apply_content_table(effective_metadata,loaded$table)
    condition<-apply_condition_table(content$metadata,loaded$condition)
    apply_ratio_table(condition$metadata,loaded$ratio)
    selected$signature_payload$rules<-normalized
    list(artifact=reloaded,normalized=normalized,inputs=selected,counts=selected$counts,
      warnings=selected$warnings,signature=signature_hash(selected$signature_payload),errors=character())
  },error=function(e)list(artifact=NULL,normalized=NULL,signature=NULL,errors=conditionMessage(e)))
  elapsed<-(proc.time()[["elapsed"]]-started)*1000
  if(length(result$errors)) logger("error","export","error",paste("Validated export preparation failed:",result$errors),details=list(elapsed_ms=elapsed))
  else logger("info","export","roundtrip",sprintf("Validated export preparation succeeded for %d metadata row(s).",nrow(effective_metadata)),details=list(elapsed_ms=elapsed))
  result
}

write_cached_rds <- function(artifact, current_signature, cached_signature, file) {
  if (is.null(artifact)) stop("No prepared export artifact is available.")
  if (is.null(current_signature) || !identical(current_signature,cached_signature))
    stop("The prepared export is stale; prepare it again.")
  saveRDS(artifact,file);reloaded<-readRDS(file)
  changed<-compare_export_objects(artifact,reloaded,values=TRUE)
  if(length(changed))stop(changed)
  invisible(reloaded)
}

# ---- UI ---------------------------------------------------------------------
table_section <- function(title, description, output, mode=c("read-only", "editable")) {
  mode <- match.arg(mode)
  label <- if (identical(mode, "editable")) "Editable" else "Read-only"
  shiny::tags$section(
    class="table-section",
    shiny::tags$div(class="table-section-heading",
      shiny::tags$h3(title),
      shiny::tags$span(class=paste("table-section-label", mode), label)),
    shiny::tags$div(class="table-section-help", description),
    output
  )
}

inference_notes <- function(title, description, output) {
  shiny::tags$section(
    class="inference-notes",
    shiny::tags$h4(title),
    shiny::tags$p(description),
    output
  )
}

build_ui <- function() shiny::fluidPage(shiny::titlePanel("MiraProt Regex Metadata Assistant"),
 shiny::tags$head(shiny::tags$style(shiny::HTML("
   .table-section { margin: 0 0 28px; }
   .table-section-heading { display: flex; align-items: baseline; gap: 10px; }
   .table-section-heading h3 { margin-bottom: 6px; }
   .table-section-label { border-radius: 3px; font-size: 11px; font-weight: 600; letter-spacing: .02em; padding: 2px 7px; text-transform: uppercase; }
   .table-section-label.editable { background: #e8f3fb; border: 1px solid #9ecae1; color: #245873; }
   .table-section-label.read-only { background: #f2f2f2; border: 1px solid #ccc; color: #555; }
   .table-section-help { background: #f7f7f7; border: 1px solid #ddd; border-left: 3px solid #aaa; border-radius: 3px; color: #555; line-height: 1.35; margin: 0 0 12px; max-height: 4.5em; max-width: 760px; overflow: auto; padding: 7px 10px; }
   .inference-notes { margin: 18px 0 24px; max-width: 760px; }
   .inference-notes h4 { margin-bottom: 4px; }
   .inference-notes p { color: #666; margin-bottom: 8px; }
   .session-lifecycle-controls { border-top: 1px solid #ddd; margin: 24px 0 28px; max-width: 760px; padding-top: 16px; }
   .session-lifecycle-controls .btn { padding: 3px 9px; }
   .session-lifecycle-controls p { color: #666; margin: 6px 0 0; }
 "))),
 shiny::sidebarLayout(shiny::sidebarPanel(shiny::fileInput("file","Excel workbook",accept=c(".xlsx",".xls")),shiny::uiOutput("sheet_ui"),shiny::uiOutput("mapping_ui"),shiny::numericInput("redundancy","Regex redundancy",0,min=0,max=10,step=1),shiny::actionButton("infer","Infer rules",class="btn-primary"),shiny::actionButton("retest","Retest edited rules"),shiny::actionButton("reset","Reset")),
 shiny::mainPanel(shiny::tabsetPanel(
  shiny::tabPanel("Workbook upload", shiny::verbatimTextOutput("dimensions"),
    table_section("Metadata preview", "Shows up to the first 100 rows from the selected worksheet before field mapping and inference.", DT::DTOutput("preview"))),
  shiny::tabPanel("Metadata validation",
    table_section("Validation results", "Lists structural errors, warnings, and informational checks. Errors must be resolved before inference can start.", DT::DTOutput("validation"))),
  shiny::tabPanel("Content-rule inference",
    inference_notes("Content inference notes", "Unresolved or weakly supported content rules are listed here.", shiny::verbatimTextOutput("content_warnings")),
    table_section("Generated content rules", "Editable MiraProt content-assignment rules. Include selects matching columns and Exclude removes unwanted matches.", DT::DTOutput("content_rules"), "editable"),
    table_section("Content-rule performance", "Reports regex size, constraint count, false positives, false negatives, precision, recall, F1, coverage, and reliability.", DT::DTOutput("content_metrics"))),
  shiny::tabPanel("Condition-rule inference",
    inference_notes("Condition inference notes", "Unresolved or weakly supported condition rules are listed here.", shiny::verbatimTextOutput("condition_warnings")),
    table_section("Generated condition rules", "Editable extraction rules used to derive condition labels independently for each content type.", DT::DTOutput("condition_rules"), "editable"),
    table_section("Condition extraction results", "Compares extracted conditions with the mapped reference labels and reports failures or unresolved rows.", DT::DTOutput("condition_diag"))),
  shiny::tabPanel("Ratio-rule inference",
    inference_notes("Ratio inference notes", "Unresolved or weakly supported ratio rules are listed here.", shiny::verbatimTextOutput("ratio_warnings")),
    table_section("Generated ratio rules", "Editable rules for extracting numerator and denominator values using a supported MiraProt ratio method.", DT::DTOutput("ratio_rules"), "editable"),
    table_section("Ratio extraction results", "Shows predicted and expected numerator and denominator values, joint success, and extraction failures.", DT::DTOutput("ratio_diag"))),
  shiny::tabPanel("Rule testing and diagnostics",
    table_section("Row-level rule results", "Shows expected and predicted assignments for each metadata row, including unmatched rows and conflicting content rules.", DT::DTOutput("row_diag")),
    table_section("Content confusion matrix", "Summarizes expected versus predicted content classifications after applying the current edited rules.", shiny::tableOutput("confusion"))),
  shiny::tabPanel("Session",
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput("session_debug_level", "Debug level",
        choices=c("0 — Summary"="0", "1 — Debug"="1", "2 — Verbose"="2"), selected="0")),
      shiny::column(8, shiny::h4(shiny::textOutput("session_active_level", inline=TRUE)),
        shiny::helpText("All bounded session entries are retained. The selected level filters both previous and future entries: Summary shows milestones, Debug adds decisions, and Verbose adds candidates and timings."))),
    table_section("Session summary", "Summarizes the workbook, worksheet, metadata size, inference state, rule counts, unresolved results, and processing time.", shiny::verbatimTextOutput("session_summary")),
    shiny::tags$section(class="session-lifecycle-controls",
      shiny::actionButton("stop_application", "Stop application", class="btn-danger btn-sm"),
      shiny::tags$p(shiny::tags$strong("Reset"), " clears workbook and inference state but keeps the application running. ",
        shiny::tags$strong("Stop application"), " closes the standalone Shiny application and allows the R process to exit.")),
    shiny::tags$details(class="table-section-help",
      shiny::tags$summary("Technical details"),
      shiny::tags$p("Representation layers: source text is losslessly tokenized; runtime PCRE atoms are compiled case-sensitively; persisted Data Wizard fields additionally escape slashes except separator alternations. Candidate families are ordered structural → shape → partial → concrete, then refined general-to-specific with deterministic quality/complexity tie-breaking."),
      shiny::tags$p(sprintf("Reliable content rules require exact persisted-form replay, F1 ≥ %.2f, recall ≥ %.2f, and no false positives. Generalization abstracts supported variation; redundancy may add only independent evidence; anchors are accepted only when coverage and quality do not decrease.", CONTENT_MIN_F1, CONTENT_MIN_RECALL)),
      shiny::tags$p("Export preserves exact table/condition/ratio column order and classes, Row Index, separator storage, and method-specific NA values; download is enabled only after RDS serialization, effective Data Wizard normalization, validation, and replay against the current metadata.")),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput("session_component_filter", "Component", choices="All")),
      shiny::column(3, shiny::selectInput("session_step_filter", "Processing step", choices="All")),
      shiny::column(4, shiny::textInput("session_text_filter", "Message text", placeholder="Filter retained entries")),
      shiny::column(2, shiny::br(), shiny::actionButton("session_clear_log", "Clear log"),
        shiny::downloadButton("session_download_log", "Download CSV"))),
    table_section("Session log", "Displays retained messages at or below the selected debug level; changing the level immediately filters both historical and future entries.", DT::DTOutput("session_log"))),
  shiny::tabPanel("Export",
    shiny::radioButtons("unreliable_action","Unreliable rules",c("Exclude from export"="exclude","Include"="include"),selected="include",inline=TRUE),
    shiny::radioButtons("unresolved_action","Unresolved rows",c("Exclude from export"="exclude","Include"="include"),inline=TRUE),
    shiny::actionButton("prepare_export","Prepare validated export",class="btn-primary"),
    shiny::verbatimTextOutput("export_status"),
    shiny::downloadButton("download_rds","Download RDS"),
    shiny::downloadButton("download_xlsx","Download review workbook"))
 ))))

# ---- server -----------------------------------------------------------------
create_session_logger <- function(log_version=NULL, threshold=.miraprot_effective_debug_level()) {
  threshold_key <- tolower(as.character(threshold)[1L])
  threshold <- if (threshold_key %in% names(MIRAPROT_LOG_LEVELS)) MIRAPROT_LOG_LEVELS[[threshold_key]] else suppressWarnings(as.integer(threshold_key))
  if (is.na(threshold)) threshold <- MIRAPROT_LOG_LEVELS[["info"]]
  threshold <- max(min(threshold, max(MIRAPROT_LOG_LEVELS)), min(MIRAPROT_LOG_LEVELS))
  state <- new.env(parent=emptyenv())
  state$entries <- list(); state$sequence <- 0L; state$dropped <- 0L; state$event_keys <- new.env(hash=TRUE, parent=emptyenv())
  state$external_active <- FALSE; state$truncation_notified <- FALSE
  bump <- function() if (is.function(log_version)) tryCatch(log_version(), error=function(e) NULL)
  append_one <- function(level="info", component="server", step="initialize", message="", event_key=NULL,
                         details=NULL, .notification=FALSE) {
    level <- tolower(as.character(level)[1L]); if (!level %in% names(MIRAPROT_LOG_LEVELS)) level <- "info"
    # Timings are Level-2 data.  Milestone records stay count-only even when a
    # caller conveniently supplies its elapsed measurement with the event.
    deferred_timing <- if (level != "trace" && is.list(details) && !is.null(details$elapsed_ms)) details$elapsed_ms else NULL
    if (!is.null(deferred_timing)) details$elapsed_ms <- NULL
    component <- as.character(component)[1L]; if (!component %in% MIRAPROT_LOG_COMPONENTS) component <- "server"
    step <- as.character(step)[1L]; if (!step %in% MIRAPROT_PROCESSING_STEPS) step <- "initialize"
    key <- if (length(event_key) && !is.na(event_key[1L]) && nzchar(as.character(event_key)[1L])) as.character(event_key)[1L] else NULL
    if (!is.null(key) && exists(key, state$event_keys, inherits=FALSE)) return(invisible(FALSE))
    original <- paste(as.character(message), collapse=" "); truncated <- nchar(original, type="chars") > MIRAPROT_MAX_LOG_MESSAGE_SIZE
    message <- substr(original, 1L, MIRAPROT_MAX_LOG_MESSAGE_SIZE)
    state$sequence <- state$sequence + 1L
    record <- list(sequence=state$sequence, timestamp=Sys.time(), level=level,
      detail_rank=unname(MIRAPROT_LOG_DETAIL_RANKS[[level]]), component=component,
      step=step, message=message, event_key=key, details=details, truncated=truncated)
    state$entries[[length(state$entries)+1L]] <- record
    if (!is.null(key)) assign(key, TRUE, state$event_keys)
    if (length(state$entries) > MIRAPROT_MAX_LOG_ENTRIES) {
      evicted <- state$entries[[1L]]; state$entries <- state$entries[-1L]
      state$dropped <- state$dropped + 1L
      if (!is.null(evicted$event_key)) rm(list=evicted$event_key, envir=state$event_keys)
    }
    # Retention is unconditional for supported levels.  Only external emission
    # is severity-thresholded, and it is intentionally decided after append.
    if (MIRAPROT_LOG_LEVELS[[level]] >= state$threshold) {
      recorder <- get0(".miraprot_log_record", envir=globalenv(), mode="function", inherits=FALSE); recorded <- FALSE
      if (is.function(recorder) && !state$external_active) {
        state$external_active <- TRUE
        recorded <- isTRUE(tryCatch({ recorder(level=level, component=component, step=step, message=message,
          event_key=key, sequence=state$sequence, details=details); TRUE }, error=function(e) FALSE))
        state$external_active <- FALSE
      }
      if (!recorded) .miraprot_console_log(level, component, step, message)
    }
    bump()
    if (!is.null(deferred_timing)) append_one("trace", component, step,
      sprintf("%s timing: %.1f ms.", step, as.numeric(deferred_timing)), details=list(elapsed_ms=deferred_timing))
    if (truncated && !state$truncation_notified && !.notification) {
      state$truncation_notified <- TRUE
      append_one("warning", "server", "initialize",
        sprintf("A log message exceeded %d characters and was truncated.", MIRAPROT_MAX_LOG_MESSAGE_SIZE),
        event_key="log-message-truncated", .notification=TRUE)
    }
    invisible(TRUE)
  }
  logger <- function(level="info", component="server", step="initialize", message="", event_key=NULL,
                     details=NULL, entries=NULL) {
    if (!is.null(entries)) {
      rows <- if (is.data.frame(entries)) split(entries, seq_len(nrow(entries))) else entries
      return(invisible(vapply(rows, function(x) do.call(append_one, x), logical(1))))
    }
    append_one(level, component, step, message, event_key, details)
  }
  attr(logger, "state") <- state
  state$threshold <- threshold
  attr(logger, "records") <- function() state$entries
  set_emission_threshold <- function(value) {
    value <- suppressWarnings(as.integer(value)[1L])
    if (is.na(value)) value <- MIRAPROT_LOG_LEVELS[["info"]]
    state$threshold <- max(min(value, max(MIRAPROT_LOG_LEVELS)), min(MIRAPROT_LOG_LEVELS))
    invisible(state$threshold)
  }
  attr(logger, "set_emission_threshold") <- set_emission_threshold
  attr(logger, "set_threshold") <- set_emission_threshold # compatibility alias
  attr(logger, "clear") <- function() {
    state$entries <- list()
    state$event_keys <- new.env(hash=TRUE, parent=emptyenv()); bump(); invisible(NULL)
  }
  attr(logger, "stats") <- function() list(retained=length(state$entries), dropped=state$dropped)
  logger
}

filter_session_log_entries <- function(entries, selected_level=0L) {
  selected_level <- suppressWarnings(as.integer(as.character(selected_level)[1L]))
  if (is.na(selected_level)) selected_level <- 0L
  selected_level <- max(0L, min(2L, selected_level))
  Filter(function(record) {
    rank <- record$detail_rank
    if (is.null(rank)) rank <- unname(MIRAPROT_LOG_DETAIL_RANKS[[record$level]])
    length(rank) == 1L && !is.na(rank) && rank <= selected_level
  }, entries)
}

session_log_frame <- function(entries) {
  if(!length(entries)) return(data.frame(Timestamp=character(),Level=character(),Component=character(),`Processing step`=character(),Message=character(),`Elapsed milliseconds`=numeric(),check.names=FALSE))
  rows<-lapply(entries,function(x)data.frame(Timestamp=format(x$timestamp,"%Y-%m-%d %H:%M:%OS3"),Level=x$level,
    Component=x$component,`Processing step`=x$step,Message=x$message,
    `Elapsed milliseconds`=if(is.list(x$details)&&!is.null(x$details$elapsed_ms))as.numeric(x$details$elapsed_ms)else NA_real_,check.names=FALSE))
  do.call(rbind,rows)
}

# Application-owned cross-session references live here rather than in the
# global environment.  Each record is deliberately small and is discarded in
# full when its session ends.
application_lifecycle <- new.env(parent=emptyenv())
application_lifecycle$app_started <- FALSE
application_lifecycle$ever_had_session <- FALSE
application_lifecycle$active_sessions <- new.env(hash=TRUE, parent=emptyenv())
application_lifecycle$next_session_id <- 0L
application_lifecycle$main_invocations <- 0L
application_lifecycle$shutdown_requested <- FALSE
application_lifecycle$shutdown_completed <- FALSE
application_lifecycle$shutdown_in_progress <- FALSE
application_lifecycle$temporary_paths <- character()
application_lifecycle$cached_artifacts <- list()
application_lifecycle$session_log_references <- list()
application_lifecycle$scheduled_callbacks <- list()
application_lifecycle$standalone_process <- FALSE
application_lifecycle$auto_stop_last_session <- TRUE
application_lifecycle$application_running <- FALSE
application_lifecycle$stop_application <- function() shiny::stopApp()

cancel_application_callbacks <- function() {
  callbacks <- application_lifecycle$scheduled_callbacks
  application_lifecycle$scheduled_callbacks <- list()
  for (callback in callbacks) {
    tryCatch({
      if (is.function(callback)) callback()
      else if (is.environment(callback) && is.function(callback$destroy)) callback$destroy()
    }, error=function(e) NULL)
  }
  invisible(NULL)
}

cleanup_application_references <- function(session_id=NULL) {
  ids <- if (is.null(session_id)) ls(application_lifecycle$active_sessions, all.names=TRUE) else as.character(session_id)
  for (id in ids) {
    record <- get0(id, envir=application_lifecycle$active_sessions, inherits=FALSE)
    if (is.null(record)) next
    if (is.function(record$clear_log)) record$clear_log()
    paths <- unique(as.character(record$temporary_files))
    paths <- paths[!is.na(paths) & nzchar(paths)]
    if (length(paths)) unlink(paths, recursive=TRUE, force=TRUE)
    # Drop cached exports, timer/observer references, log accessors, and any
    # other session-owned closures together by removing the complete record.
    rm(list=id, envir=application_lifecycle$active_sessions)
  }
  invisible(NULL)
}

shutdown_application <- function(reason, stop_app=TRUE) {
  if (isTRUE(application_lifecycle$shutdown_completed) ||
      isTRUE(application_lifecycle$shutdown_in_progress)) return(invisible(FALSE))
  application_lifecycle$shutdown_requested <- TRUE
  application_lifecycle$shutdown_in_progress <- TRUE
  on.exit({
    application_lifecycle$shutdown_completed <- TRUE
    application_lifecycle$shutdown_in_progress <- FALSE
  }, add=TRUE)
  cancel_application_callbacks()
  application_lifecycle$cached_artifacts <- list()
  application_lifecycle$session_log_references <- list()
  cleanup_application_references()
  paths <- unique(as.character(application_lifecycle$temporary_paths))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (length(paths)) unlink(paths, recursive=TRUE, force=TRUE)
  application_lifecycle$temporary_paths <- character()
  .miraprot_bootstrap_log("info", "server", "shutdown",
    paste("Regex Metadata Assistant application shutdown:", as.character(reason)[1L]),
    "application-shutdown")
  if (isTRUE(stop_app) && isTRUE(application_lifecycle$application_running)) {
    tryCatch(application_lifecycle$stop_application(), error=function(e) {
      # stopApp() reports this when its session has already unwound.  Cleanup is
      # deliberately silent here to avoid routing an error back into logging.
      if (!grepl("no .*application.*running|application is not .*running",
                 conditionMessage(e), ignore.case=TRUE))
        cat("MiraProt stopApp warning: ", conditionMessage(e), "\n", sep="", file=stderr())
    })
  }
  invisible(TRUE)
}

end_application_session <- function(session_id, logger=.miraprot_noop_logger,
                                    per_session_cleanup=function() NULL) {
  id <- as.character(session_id)
  record <- get0(id, envir=application_lifecycle$active_sessions, inherits=FALSE)
  if (is.null(record) || isTRUE(record$ended)) return(invisible(FALSE))
  record$ended <- TRUE
  logger("info", "server", "shutdown", "Regex Metadata Assistant session ended.",
    paste0("session-ended-", id))
  rm(list=id, envir=application_lifecycle$active_sessions)
  tryCatch(per_session_cleanup(), error=function(e)
    cat("MiraProt session cleanup warning: ", conditionMessage(e), "\n", sep="", file=stderr()))
  # This is intentionally edge-triggered by a registered session ending.  In
  # particular, an empty registry at startup is not evidence that the app is
  # idle and must never initiate shutdown.
  if (isTRUE(application_lifecycle$standalone_process) &&
      isTRUE(application_lifecycle$auto_stop_last_session) &&
      isTRUE(application_lifecycle$ever_had_session) &&
      !length(ls(application_lifecycle$active_sessions, all.names=TRUE)) &&
      !isTRUE(application_lifecycle$shutdown_requested))
    shutdown_application("last session ended")
  invisible(TRUE)
}

server <- function(input,output,session){
 setup_complete<-FALSE
 rv<-shiny::reactiveValues(sheets=NULL,data=NULL,names=NULL,rules=NULL,metrics=NULL,warnings=NULL,applied=NULL,busy=FALSE,log_version=0L,
   export_artifact=NULL,export_errors=character(),export_status="not_prepared",export_signature=NULL,
   export_started=NULL,export_elapsed=NULL,export_busy=FALSE,export_counts=NULL)
 session_started<-Sys.time()
 logger<-create_session_logger(function() rv$log_version<-rv$log_version+1L)
 application_lifecycle$next_session_id<-application_lifecycle$next_session_id+1L
 session_id<-as.character(application_lifecycle$next_session_id)
 lifecycle_record<-new.env(parent=emptyenv())
 lifecycle_record$clear_log<-attr(logger,"clear")
 lifecycle_record$temporary_files<-character()
 lifecycle_record$timers<-list()
 lifecycle_record$cached_export<-function()rv$export_artifact
 assign(session_id,lifecycle_record,envir=application_lifecycle$active_sessions)
 application_lifecycle$ever_had_session<-TRUE
 on.exit(if(!setup_complete)end_application_session(session_id,logger,function()
   cleanup_application_references(session_id)),add=TRUE)
 application_lifecycle$session_log_references[[session_id]]<-logger
 session$userData$miraprot_logger<-logger
 session$userData$miraprot_log_records<-attr(logger,"records")
 session$userData$miraprot_export_state<-rv
 logger("info","server","initialize","Regex Metadata Assistant session initialized.","session-initialized")
 # Levels are user-facing detail settings: Summary=info+, Debug=debug+, Verbose=trace+.
 shiny::observeEvent(input$session_debug_level, {
   selected<-as.integer(input$session_debug_level); threshold<-c(2L,1L,0L)[selected+1L]
   attr(logger,"set_emission_threshold")(threshold)
 }, ignoreInit=FALSE)
 output$session_active_level<-shiny::renderText({
   labels<-c("0 — Summary","1 — Debug","2 — Verbose"); selected<-input$session_debug_level
   if(is.null(selected))selected<-"0"; labels[as.integer(selected)+1L]
 })
 shiny::observeEvent(input$session_clear_log, { attr(logger,"clear")() })
 log_snapshot<-shiny::reactive({
   rv$log_version; selected<-input$session_debug_level; component<-input$session_component_filter; step<-input$session_step_filter; text<-input$session_text_filter
   if(is.null(selected))selected<-0L
   frame<-session_log_frame(filter_session_log_entries(attr(logger,"records")(),selected))
   if(!is.null(component)&&component!="All")frame<-frame[frame$Component==component,,drop=FALSE]
   if(!is.null(step)&&step!="All")frame<-frame[frame[["Processing step"]]==step,,drop=FALSE]
   if(!is.null(text)&&nzchar(text))frame<-frame[grepl(text,frame$Message,fixed=TRUE,ignore.case=TRUE),,drop=FALSE]
   frame
 })
 shiny::observe({rv$log_version; component<-isolate(input$session_component_filter);step<-isolate(input$session_step_filter);if(is.null(component))component<-"All";if(is.null(step))step<-"All";shiny::updateSelectInput(session,"session_component_filter",choices=c("All",MIRAPROT_LOG_COMPONENTS),selected=component);shiny::updateSelectInput(session,"session_step_filter",choices=c("All",MIRAPROT_PROCESSING_STEPS),selected=step)})
 output$session_log<-DT::renderDT({x<-log_snapshot();if(nrow(x))x<-x[rev(seq_len(nrow(x))),,drop=FALSE];DT::datatable(x,rownames=FALSE,options=list(pageLength=25,deferRender=TRUE,scrollX=TRUE,order=list(list(0,"desc"))))})
 output$session_download_log<-shiny::downloadHandler(function()paste0("MiraProt_session_log_",Sys.Date(),".csv"),function(file){write.csv(session_log_frame(attr(logger,"records")()),file,row.names=FALSE,na="")})
 output$session_summary<-shiny::renderText({
   shiny::invalidateLater(1000,session);rv$log_version
   workbook<-if(is.null(input$file$name))"Not uploaded"else input$file$name
   worksheet<-if(is.null(input$sheet))"Not selected"else input$sheet
   dimensions<-if(is.null(rv$data))"Not loaded"else sprintf("%d rows x %d columns",nrow(rv$data),ncol(rv$data))
   validation<-if(is.null(rv$data))"Not run"else tryCatch({v<-validate_metadata(mapped(),rv$names,if(!is.null(input$condition_map)&&!startsWith(input$condition_map,"--"))input$condition_map else "");if(any(v$Severity=="Error"))"Errors"else if(any(v$Severity=="Warning"))"Warnings"else"Valid"},error=function(e)"Unavailable")
   inference<-if(isTRUE(rv$busy))"Running"else if(is.null(rv$rules))"Not run"else"Completed"
   counts<-if(is.null(rv$rules))c(0L,0L,0L)else vapply(rv$rules,nrow,integer(1))[c("table","condition","ratio")]
   status_sets<-if(is.null(rv$metrics))NULL else rv$metrics$rule_status
   unresolved<-if(!is.list(status_sets))0L else sum(vapply(status_sets,function(z) {
     if(!is.data.frame(z))return(0L)
     if("Status"%in%names(z))return(sum(tolower(chr(z$Status))%in%c("unresolved","provisional","conflicting"),na.rm=TRUE))
     if("Reliable"%in%names(z))return(sum(!z$Reliable,na.rm=TRUE))
     0L
   },integer(1)))
   not_applicable<-if(!is.list(status_sets))0L else sum(vapply(status_sets,function(z)
     if(is.data.frame(z)&&"Status"%in%names(z))sum(tolower(chr(z$Status))=="not_applicable",na.rm=TRUE)else 0L,integer(1)))
   stats<-attr(logger,"stats")()
   content_summary<-if(is.null(rv$rules)||is.null(rv$data))NULL else
     tryCatch({application<-apply_content_table(mapped(),rv$rules$table);content_assignment_summary(application$rows,rv$rules$table)},error=function(e)NULL)
   content_lines<-if(is.null(content_summary))c(
     "Assigned correctly: 0","False positives: 0","False negatives: 0","Unresolved rows: 0","Conflicts: 0","Labels with no selected rule: 0") else c(
     sprintf("Assigned correctly: %d",content_summary$assigned_correctly),
     sprintf("False positives: %d",content_summary$false_positives),
     sprintf("False negatives: %d",content_summary$false_negatives),
     sprintf("Unresolved rows: %d",content_summary$unresolved_rows),
     sprintf("Conflicts: %d",content_summary$conflicts),
     sprintf("Labels with no selected rule: %d",content_summary$labels_with_no_selected_rule))
   paste(sprintf("Workbook: %s",workbook),sprintf("Worksheet: %s",worksheet),sprintf("Metadata dimensions: %s",dimensions),
     sprintf("Validation status: %s",validation),sprintf("Inference status: %s",inference),
     sprintf("Elapsed time: %.1f seconds",as.numeric(difftime(Sys.time(),session_started,units="secs"))),
     sprintf("Generated rules: content %d; condition %d; ratio %d; total %d",counts[1],counts[2],counts[3],sum(counts)),
     content_lines,sprintf("Unresolved rule count: %d",unresolved),sprintf("Not-applicable condition count: %d",not_applicable),sprintf("Retained log entries: %d",stats$retained),
     sprintf("Dropped log entries: %d",stats$dropped),sep="\n")
 })
 shiny::observeEvent(input$file,{started<-proc.time()[["elapsed"]];logger("info","workbook","upload",sprintf("Workbook upload received: '%s'.",.miraprot_safe_value(input$file$name)),"workbook-upload");rv$sheets<-tryCatch(readxl::excel_sheets(input$file$datapath),error=function(e){logger("error","workbook","error",paste("Workbook inspection failed:",e$message));shiny::showNotification(e$message,type="error");NULL});if(!is.null(rv$sheets))logger("info","workbook","inspect",sprintf("Workbook inspection found %d sheet(s): %s.",length(rv$sheets),paste(head(rv$sheets,3L),collapse=", ")),details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))})
 output$sheet_ui<-shiny::renderUI(if(length(rv$sheets))shiny::selectInput("sheet","Metadata worksheet",choices=rv$sheets))
 shiny::observeEvent(input$sheet,{started<-proc.time()[["elapsed"]];d<-tryCatch(readxl::read_excel(input$file$datapath,sheet=input$sheet,.name_repair="minimal"),error=function(e){logger("error","workbook","error",paste("Sheet loading failed:",e$message));shiny::showNotification(e$message,type="error");NULL});if(!is.null(d)){rv$names<-names(d);rv$data<-as.data.frame(d,check.names=FALSE);logger("info","workbook","load",sprintf("Loaded sheet '%s': %d rows and %d columns.",.miraprot_safe_value(input$sheet),nrow(d),ncol(d)),details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))}})
 output$mapping_ui<-shiny::renderUI({if(is.null(rv$data))return(NULL);n<-names(rv$data);shiny::tagList(shiny::selectInput("column_map","Source Column field",c("-- unmapped --",n),selected=if("Column"%in%n)"Column"else"-- unmapped --"),shiny::selectInput("content_map","Content target",c("-- unavailable --",n),selected=if("Content"%in%n)"Content"else"-- unavailable --"),shiny::selectInput("condition_map","Condition target (Options is MiraProt's assigned condition)",c("-- unavailable --",n),selected=if("Options"%in%n)"Options"else"-- unavailable --"))})
 mapped<-shiny::reactive({d<-rv$data;shiny::req(d);mappings<-list(c(input$column_map,"Column"),c(input$content_map,"Content"));for(z in mappings)if(!is.null(z[[1]])&&!startsWith(z[[1]],"--")&&z[[1]]!=z[[2]])d[[z[[2]]]]<-d[[z[[1]]]];d})
 output$dimensions<-shiny::renderText(if(is.null(rv$data))"Upload a workbook." else sprintf("%d rows x %d columns; sheet: %s",nrow(rv$data),ncol(rv$data),input$sheet));output$preview<-DT::renderDT(if(!is.null(rv$data))head(rv$data,WORKBOOK_PREVIEW_ROW_LIMIT),options=list(scrollX=TRUE))
 output$validation<-DT::renderDT(if(!is.null(rv$data))validate_metadata(mapped(),rv$names,if(!is.null(input$condition_map)&&!startsWith(input$condition_map,"--"))input$condition_map else ""),options=list(dom="t",scrollX=TRUE))
 shiny::observeEvent(input$infer,{if(rv$busy)return();rv$busy<-TRUE;on.exit(rv$busy<-FALSE);started<-proc.time()[["elapsed"]];d<-mapped();v<-validate_metadata(d,rv$names,if(startsWith(input$condition_map,"--"))""else input$condition_map);logger("info","server","validate",sprintf("Metadata validation completed: %d errors and %d warnings.",sum(v$Severity=="Error"),sum(v$Severity=="Warning")));if(any(v$Severity=="Error")){logger("warning","server","validate","Inference blocked by validation errors.");shiny::showNotification("Resolve validation errors before inference.",type="error");return()};tryCatch(shiny::withProgress(message="Inferring and validating rules",value=0,{logger("trace","server","preprocess",sprintf("Preprocessing %d mapped row(s); representative row IDs: %s.",nrow(d),paste(head(seq_len(nrow(d)),3L),collapse=",")));c<-if("Content"%in%names(d))infer_content(d,input$redundancy,logger)else list(table=empty_content(),metrics=data.frame(),warnings="Content unavailable.");shiny::incProgress(.4);q<-infer_conditions(d,if(startsWith(input$condition_map,"--"))""else input$condition_map,logger);shiny::incProgress(.3);r<-infer_ratios(d,logger);rv$rules<-coerce_contract(list(table=c$table,condition=q$table,ratio=r$table));rv$metrics<-list(content=c$metrics,condition=q$diagnostics,ratio=r$diagnostics,rule_status=list(table=c$status,condition=q$status,ratio=r$status));rv$warnings<-list(content=c$warnings,condition=q$warnings,ratio=r$warnings);logger("info","server","infer",sprintf("Rule inference completed: %d content, %d condition, %d ratio rules.",nrow(c$table),nrow(q$table),nrow(r$table)),details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000));shiny::incProgress(.3)}),error=function(e){logger("error","server","error",paste("Fatal inference error:",e$message));shiny::showNotification(e$message,type="error")})})
 editable<-function(name,key){output[[name]]<-DT::renderDT(if(is.null(rv$rules))data.frame()else rv$rules[[key]],editable=TRUE,options=list(scrollX=TRUE));shiny::observeEvent(input[[paste0(name,"_cell_edit")]],{e<-input[[paste0(name,"_cell_edit")]];x<-rv$rules[[key]];field<-names(x)[e$col];old<-x[e$row,e$col][[1L]];new<-DT::coerceValue(e$value,x[e$row,e$col]);x[e$row,e$col]<-new;rv$rules[[key]]<-x;logger("debug","server","edit",sprintf("Manual edit table=%s row=%d field=%s old='%s' new='%s'.",key,e$row,field,.miraprot_safe_value(old),.miraprot_safe_value(new)));if(key=="table"&&field=="Transformation")logger("info","content","edit",sprintf("%s: Transformation=%s; source=manual resolution.",.miraprot_safe_value(x$Content[e$row]),.miraprot_safe_value(new)))})};editable("content_rules","table");editable("condition_rules","condition");editable("ratio_rules","ratio")
 output$content_metrics<-DT::renderDT(rv$metrics$content);output$condition_diag<-DT::renderDT(rv$metrics$condition);output$ratio_diag<-DT::renderDT(rv$metrics$ratio);output$content_warnings<-shiny::renderText(paste(rv$warnings$content,collapse="\n"));output$condition_warnings<-shiny::renderText(paste(rv$warnings$condition,collapse="\n"));output$ratio_warnings<-shiny::renderText(paste(rv$warnings$ratio,collapse="\n"))
 diagnostics<-shiny::reactive({if(!is.null(rv$applied))rv$applied$content$rows else if(is.null(rv$rules))data.frame()else test_rules(mapped(),rv$rules)});output$row_diag<-DT::renderDT(diagnostics(),options=list(scrollX=TRUE));output$confusion<-shiny::renderTable({z<-diagnostics();if(nrow(z))table(Expected=z$Expected,Predicted=z$Predicted,useNA="ifany")})
 shiny::observeEvent(input$retest,{
   started<-proc.time()[["elapsed"]];logger("info","server","retest","Retest requested; validating edited rules before application.")
   if(is.null(rv$rules)){logger("warning","server","retest","Retest skipped: no edited rules exist.");shiny::showNotification("No edited rules to retest.",type="error");return()}
   invalid_condition_messages<-condition_content_validation_messages(rv$rules$condition)
   retest_rules<-rv$rules
   retest_rules$condition<-retest_rules$condition[is_sample_bearing_content(retest_rules$condition$Content),,drop=FALSE]
   if(length(invalid_condition_messages)) shiny::showNotification(paste(invalid_condition_messages,collapse=" "),type="warning",duration=NULL)
   schema_errors<-validate_export(retest_rules,logger=logger)
   if(length(schema_errors)){logger("warning","server","retest",sprintf("Retest blocked by %d raw-schema error(s).",length(schema_errors)));shiny::showNotification(paste(schema_errors,collapse=" "),type="error");return()}
   rules<-tryCatch(coerce_contract(retest_rules),error=function(e){shiny::showNotification(paste("Edited rule values are invalid:",e$message),type="error");NULL})
   if(is.null(rules))return()
   regex_errors<-validate_export(rules,logger=logger)
   if(length(regex_errors)){shiny::showNotification(paste(regex_errors,collapse=" "),type="error");return()}
   d<-mapped();expected_condition<-if(!is.null(input$condition_map)&&!startsWith(input$condition_map,"--")&&input$condition_map%in%names(d))chr(d[[input$condition_map]])else rep("",nrow(d));expected_num<-if("Numerator"%in%names(d))chr(d$Numerator)else rep("",nrow(d));expected_den<-if("Denominator"%in%names(d))chr(d$Denominator)else rep("",nrow(d))
   applied<-tryCatch({content<-apply_content_table(d,rules$table);condition<-apply_condition_table(content$metadata,rules$condition,expected_condition);ratio<-apply_ratio_table(condition$metadata,rules$ratio,expected_num,expected_den);list(content=content,condition=condition,ratio=ratio)},error=function(e){logger("error","server","error",paste("Recoverable retest application error:",e$message));shiny::showNotification(paste("Rule application failed:",e$message),type="error");NULL})
   if(is.null(applied))return()
   rv$applied<-applied;rv$metrics$content<-applied$content$metrics;rv$metrics$condition<-applied$condition$diagnostics;rv$metrics$ratio<-applied$ratio$diagnostics
   rows<-applied$content$rows;rv$warnings$content<-c(sprintf("Unresolved rows: %d",sum(rows$Unresolved)),sprintf("False positives: %d",sum(rows$Predicted!=rows$Expected&nzchar(rows$Predicted))),sprintf("False negatives: %d",sum(rows$Predicted!=rows$Expected&nzchar(rows$Expected))),sprintf("Overlapping content matches: %d",sum(rows$Conflict)))
   rv$warnings$condition<-sprintf("Condition extraction failures: %d",nrow(applied$condition$failures));rv$warnings$ratio<-sprintf("Ratio extraction failures: %d",nrow(applied$ratio$failures))
   logger("info","server","retest",sprintf("Retest applied %d rules to %d rows: content FP=%d FN=%d conflicts=%d; condition failures=%d; ratio failures=%d.",sum(vapply(rules,nrow,integer(1))),nrow(d),sum(rows$Predicted!=rows$Expected&nzchar(rows$Predicted)),sum(rows$Predicted!=rows$Expected&nzchar(rows$Expected)),sum(rows$Conflict),nrow(applied$condition$failures),nrow(applied$ratio$failures)),details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
   shiny::showNotification(sprintf("Applied %d content, %d condition, and %d ratio rules to %d metadata rows.",nrow(rules$table),nrow(rules$condition),nrow(rules$ratio),nrow(d)),type="message")
 })
 shiny::observeEvent(input$reset,{rv$data<-rv$rules<-rv$metrics<-rv$warnings<-rv$applied<-NULL;rv$sheets<-NULL;shiny::updateNumericInput(session,"redundancy",value=0);shiny::updateRadioButtons(session,"unreliable_action",selected="include");shiny::updateRadioButtons(session,"unresolved_action",selected="exclude");logger("info","server","reset","Session workflow state reset; retained session log unchanged.")})
 shiny::observeEvent(input$stop_application,{
   shiny::showModal(shiny::modalDialog(
     title="Stop application?",
     "Stopping closes the standalone application. Unsaved rules and session logs will be lost, and the R process will be allowed to exit.",
     footer=shiny::tagList(shiny::modalButton("Cancel"),
       shiny::actionButton("confirm_stop_application","Stop application",class="btn-danger")),
     easyClose=FALSE
   ))
 },ignoreInit=TRUE)
 shiny::observeEvent(input$confirm_stop_application,{
   logger("info","server","shutdown","Application shutdown requested by the user.",
     "application-shutdown-request")
   shutdown_application("user requested shutdown")
 },ignoreInit=TRUE,once=TRUE)
 invalidate_export<-function(){rv$export_artifact<-NULL;rv$export_errors<-character();rv$export_status<-"not_prepared";rv$export_signature<-NULL;rv$export_counts<-NULL;rv$export_started<-NULL;rv$export_elapsed<-NULL}
 # These are precisely the inputs from which preparation is derived.  Merely
 # invalidating never performs normalization, validation, construction or I/O.
 shiny::observeEvent(list(input$file,input$sheet,input$column_map,input$content_map,input$condition_map,
   input$redundancy,rv$rules,rv$metrics,input$unreliable_action,input$unresolved_action),invalidate_export(),ignoreInit=TRUE)
 inclusion_settings<-function()list(unreliable_action=input$unreliable_action,
   unresolved_action=input$unresolved_action)
 export_diagnostics<-function()list(rule_status=if(is.null(rv$metrics))NULL else rv$metrics$rule_status,
   rows=if(!is.null(rv$applied))rv$applied$content$rows else NULL)
 export_input_preview<-function(){
   if(is.null(rv$rules)||is.null(rv$data))return(NULL)
   tryCatch(prepare_export_inputs(rv$rules,export_diagnostics(),mapped(),input$unreliable_action,input$unresolved_action),error=function(e)NULL)
 }
 current_export_signature<-function(){
   if(is.null(rv$rules)||is.null(rv$data))return(NULL)
   tryCatch({selected<-export_input_preview();selected$signature_payload$rules<-coerce_contract(selected[c("table","condition","ratio")]);signature_hash(selected$signature_payload)},error=function(e)NULL)
 }
 shiny::observeEvent(input$prepare_export,{
   if(isTRUE(rv$export_busy))return()
   rv$export_busy<-TRUE;on.exit(rv$export_busy<-FALSE,add=TRUE)
   rv$export_artifact<-NULL;rv$export_errors<-character();rv$export_signature<-NULL
   rv$export_status<-"preparing";rv$export_started<-Sys.time();started<-proc.time()[["elapsed"]]
   metadata<-if(is.null(rv$data))NULL else mapped()
   export_metrics<-export_diagnostics()
   result<-prepare_export_artifact(rv$rules,metadata,inclusion_settings(),logger,metrics=export_metrics)
   rv$export_elapsed<-(proc.time()[["elapsed"]]-started)
   if(length(result$errors)){rv$export_errors<-result$errors;rv$export_status<-"failed"}
   else{rv$export_artifact<-result$artifact;rv$export_signature<-result$signature;rv$export_counts<-result$counts;rv$export_status<-"ready"}
 },ignoreInit=TRUE)
 output$export_status<-shiny::renderText({
   status<-rv$export_status;errors<-rv$export_errors
   counts<-if(identical(status,"ready"))rv$export_counts else {preview<-export_input_preview();if(is.null(preview))NULL else preview$counts}
   summary<-if(is.null(counts))"" else sprintf("\nRules included: content %d, condition %d, ratio %d; unreliable excluded: %d. Metadata included: %d; unresolved excluded: %d.",counts$rules$included[["table"]],counts$rules$included[["condition"]],counts$rules$included[["ratio"]],sum(counts$rules$excluded_unreliable),counts$metadata$included,counts$metadata$excluded_unresolved)
   if(identical(status,"preparing"))"Preparing, validating, replaying, and round-tripping the export..."
   else if(identical(status,"ready"))paste0("Ready: the cached artifact was validated, replayed against current metadata, and round-tripped successfully.",summary)
   else if(identical(status,"failed"))paste("Preparation failed:",paste(errors,collapse="\n"))
   else paste0("Not prepared. Click 'Prepare validated export' after confirming the inclusion settings.",summary)
 })
 guarded_download<-function(file,kind){
   outcome<-tryCatch({
     if(!identical(rv$export_status,"ready")||is.null(rv$export_artifact))stop("No prepared export artifact is available.")
     signature<-current_export_signature()
     if(is.null(signature)||!identical(signature,rv$export_signature))stop("The prepared export is stale; prepare it again.")
     artifact<-rv$export_artifact
     if(identical(kind,"rds")){
       write_cached_rds(artifact,signature,rv$export_signature,file)
     }else{
       check_path<-tempfile(fileext=".rds");lifecycle_record$temporary_files<-c(lifecycle_record$temporary_files,check_path)
       on.exit({unlink(check_path);lifecycle_record$temporary_files<-setdiff(lifecycle_record$temporary_files,check_path)},add=TRUE)
       saveRDS(artifact,check_path);reloaded<-readRDS(check_path)
       changed<-compare_export_objects(artifact,reloaded,values=FALSE);if(length(changed))stop(changed)
       openxlsx::write.xlsx(artifact[c("table","condition","ratio")],file,overwrite=TRUE)
       if(!identical(openxlsx::getSheetNames(file),c("table","condition","ratio")))stop("Review workbook schema verification failed.")
     }
     sprintf("%s download completed: %d bytes written from the cached validated artifact.",toupper(kind),file.info(file)$size)
   },error=function(e)e)
   if(inherits(outcome,"error")){logger("error","export","download",paste(toupper(kind),"download failed:",conditionMessage(outcome)));stop(outcome)}
   logger("info","export","download",outcome)
 }
 output$download_rds<-shiny::downloadHandler(function()paste0("MiraProt_assignment_rules_",Sys.Date(),".rds"),function(file)guarded_download(file,"rds"))
 output$download_xlsx<-shiny::downloadHandler(function()paste0("MiraProt_rule_review_",Sys.Date(),".xlsx"),function(file)guarded_download(file,"xlsx"))
 session$onSessionEnded(function() {
   end_application_session(session_id,logger,function(){
     for(timer in lifecycle_record$timers)tryCatch({
       if(is.function(timer))timer()
       else if(is.environment(timer)&&is.function(timer$destroy))timer$destroy()
     },error=function(e)NULL)
     lifecycle_record$timers<-list()
     rv$export_artifact<-NULL;rv$rules<-NULL;rv$data<-NULL;rv$applied<-NULL
     session$userData$miraprot_logger<-NULL
     session$userData$miraprot_log_records<-NULL
     session$userData$miraprot_export_state<-NULL
     application_lifecycle$session_log_references[[session_id]]<-NULL
     if (is.function(lifecycle_record$clear_log)) lifecycle_record$clear_log()
     paths<-unique(lifecycle_record$temporary_files)
     if(length(paths))unlink(paths,recursive=TRUE,force=TRUE)
   })
 })
 setup_complete<-TRUE
}

# Optional non-interactive smoke tests remain in this single file.
run_self_tests <- function(){
  private_bindings <- c("required_packages","package_repository","MIRAPROT_LOG_LEVELS",
    "MIRAPROT_LOG_COMPONENTS","ensure_packages","regex_escape_literal","infer_content",
    "build_ui","server","application_lifecycle","cleanup_application_references",
    "run_self_tests","main")
  stopifnot(!any(vapply(private_bindings, exists, logical(1), envir=globalenv(), inherits=FALSE)))
  # Permanent regression guard: inspect this complete script (including its UI
  # declarations) so the removed export gate cannot be reintroduced anywhere.
  source_candidates <- c(getSrcFilename(run_self_tests, full.names=TRUE),
    file.path(getwd(), "Regex_Metadata_Assistant.R"))
  source_path <- source_candidates[file.exists(source_candidates)][1L]
  stopifnot(length(source_path)==1L, !is.na(source_path))
  script_definition <- paste(readLines(source_path, warn=FALSE), collapse="\n")
  removed_gate_terms <- c(paste0("ack_", "unreliable"),
    paste("affirmatively", "acknowledge"),
    paste("requires affirmative", "acknowledgment"))
  stopifnot(!any(vapply(removed_gate_terms, grepl, logical(1),
    x=script_definition, ignore.case=TRUE, fixed=TRUE)))
  # Policy guards deliberately inspect executable acceptance logic as well as
  # user-facing prose.  Keep the searched expressions assembled in pieces so
  # the guards do not match their own declarations.
  acceptance_definition <- paste(vapply(list(infer_content,infer_conditions,infer_ratios),
    function(fun)paste(deparse(body(fun)),collapse="\n"),character(1)),collapse="\n")
  minimum_two_patterns <- c(paste0("length\\s*\\([^)]*\\)\\s*[>]", "=\\s*2"),
    paste0("nrow\\s*\\([^)]*\\)\\s*[>]", "=\\s*2"),
    paste0("sum\\s*\\([^)]*\\)\\s*[>]", "=\\s*2"),
    paste0("length\\s*\\([^)]*\\)\\s*[<]", "\\s*2"),
    paste0("nrow\\s*\\([^)]*\\)\\s*[<]", "\\s*2"))
  stopifnot(!any(vapply(minimum_two_patterns,grepl,logical(1),
    x=acceptance_definition,perl=TRUE)))
  prohibited_claims <- c(paste("at least", "two positive examples"),
    paste("one-example memorization", "is not reliable"))
  stopifnot(!any(vapply(prohibited_claims,grepl,logical(1),x=script_definition,
    ignore.case=TRUE,fixed=TRUE)))
  ack_name_pattern <- paste0("^",paste0("acknowledg","[[:alnum:]_.]*"))
  parsed_names <- all.names(parse(source_path),functions=TRUE,unique=TRUE)
  ack_control_pattern <- paste0("(?:checkboxInput|actionButton|radioButtons|reactiveVal|reactiveValues|updateCheckboxInput)",
    "\\s*\\([^)]*['\"]",paste0("acknowledg","[[:alnum:]_.-]*"))
  stopifnot(!any(grepl(ack_name_pattern,parsed_names,ignore.case=TRUE,perl=TRUE)),
    !grepl(ack_control_pattern,script_definition,ignore.case=TRUE,perl=TRUE))
  ui_definition<-as.character(build_ui())
  stopifnot(grepl('id="unreliable_action"',ui_definition,fixed=TRUE),
    grepl('value="include" checked',ui_definition,fixed=TRUE),
    grepl('updateRadioButtons(session, "unreliable_action", selected = "include")',
      paste(deparse(body(server)),collapse=" "),fixed=TRUE))
  stopifnot(regex_escape_literal("a+b.c$")=="a\\+b\\.c\\$",
    validate_pcre("(?=.*a)b|c")$valid, !validate_pcre("(")$valid)
  validation <- validate_pcre("a+")
  stopifnot(identical(names(validation), c("valid","engine","message","length","complexity")),
    validation$valid, validation$engine == "PCRE", validation$length == 2L,
    validate_stringr_pattern("a+")$valid)
  stopifnot(regex_atom_for_token("  ", "exact") == "  ",
    regex_atom_for_token("  ", "one_or_more") == "\\s+",
    regex_atom_for_token("  ", "optional") == "\\s*",
    regex_atom_for_token(" ", evidence=c(" ", "  ")) == "\\s+",
    regex_atom_for_token(" ", evidence=c("", " ")) == "\\s*")
  # Completion gate: every supported literal metacharacter, including a
  # literal backslash and slash, survives compilation, storage, RDS reload and
  # application. Anchors are added only after the literal atom is constructed.
  literal <- "\\.^$|()[]{}*+?/"
  atom <- regex_atom_for_token(literal, "exact")
  anchored <- regex_join_atoms(c("^", atom, "$"))
  stored <- regex_to_miraprot_storage(anchored, "content")
  stopifnot(validate_pcre(stored)$valid, grepl(stored, literal, perl=TRUE),
    !grepl(stored, paste0("x", literal), perl=TRUE),
    identical(regex_from_miraprot_storage(stored, "content"), anchored),
    identical(regex_to_miraprot_storage(stored, "content"), stored),
    identical(regex_to_miraprot_storage("/|\\s+", "separator"), "/|\\s+"))
  gate_file <- tempfile(fileext=".rds"); on.exit(unlink(gate_file), add=TRUE)
  saveRDS(stored, gate_file); reloaded <- readRDS(gate_file)
  gate_rules <- data.frame(Content="Literal", Include=reloaded, Exclude="", Transformation="None", check.names=FALSE)
  gate_applied <- apply_content_table(data.frame(Column=c(literal,paste0("x",literal))), gate_rules)
  stopifnot(identical(reloaded, stored), identical(gate_applied$metadata$Content, c("Literal", "")))
  lex_sources <- c("Ab12 (R3)\t9x,§", "Cd34 (R4)\t8y,§")
  lexed <- tokens(lex_sources)
  rebuilt <- vapply(split(lexed$Text, lexed$Source), paste0, collapse="", FUN.VALUE=character(1))
  contiguous <- unlist(lapply(split(seq_len(nrow(lexed)),lexed$Source),function(rows)
    length(rows) < 2L || all(lexed$Start[rows[-1L]] == lexed$End[rows[-length(rows)]]+1L)))
  stopifnot(identical(unname(rebuilt),lex_sources),
    all(lexed$Start[c(TRUE, diff(lexed$Source) != 0L)] == 1L),
    all(contiguous),
    all(lexed$End-lexed$Start+1L == nchar(lexed$Text)),
    any(lexed$Type == "whitespace"), any(lexed$Type == "paren_open"),
    any(lexed$Type == "paren_close"), any(lexed$Type == "comma"),
    any(lexed$Type == "unknown"), any(lexed$Shape == "prefix_numeric_suffix"),
    any(lexed$Shape == "numeric_prefix_letters"), any(lexed$ReplicateLike),
    any(lexed$Parenthesized), any(lexed$ConditionLike),
    identical(lexed$Text, c("Ab12"," ","(","R3",")","\t","9x",",","§",
                            "Cd34"," ","(","R4",")","\t","8y",",","§")))
  stopifnot(nrow(tokens("")) == 0L)
  # Candidate-family completion gate: aligned examples retain invariant class
  # terms and punctuation, abstract genuinely varying values, and still expose
  # progressively more concrete fallbacks within both family/global bounds.
  family_examples <- c("Intensity SampleA (R1)", "Intensity SampleB (R2)",
                       "Intensity SampleC (R3)")
  family_negatives <- c("Abundance SampleA (R1)", "SampleA (R1)")
  family_candidates <- candidate_fragments(family_examples,family_negatives)
  structural_at <- which(grepl("[[:alpha:]]",family_candidates,fixed=TRUE))[1L]
  partial_at <- which(family_candidates == "Intensity")[1L]
  concrete_at <- which(family_candidates == regex_to_miraprot_storage(
    regex_atom_for_token(family_examples[[1L]]),"content"))[1L]
  stopifnot(length(family_candidates) <= CANDIDATE_FRAGMENT_SEARCH_LIMIT,
    !is.na(structural_at), !is.na(partial_at), !is.na(concrete_at),
    structural_at < partial_at, partial_at < concrete_at,
    any(grepl("Intensity",family_candidates)),
    !any(family_candidates == "[[:alpha:]]+"))
  optional_candidates <- candidate_fragments(c("Reporter Intensity R1",
    "Reporter normalized Intensity R2"), "Reporter Error R1")
  stopifnot(any(grepl(")?",optional_candidates,fixed=TRUE)),
    length(optional_candidates) <= CANDIDATE_FRAGMENT_SEARCH_LIMIT)
  scored<-score_pattern("^raw",c("raw_A","raw_B","ratio_A_B"),c(TRUE,TRUE,FALSE))
  stopifnot(identical(names(scored)[1:13],c("TP","TN","FP","FN","Precision",
    "Recall","F1","Specificity","BalancedAccuracy","Coverage","RegexLength",
    "Complexity","ConstraintCount")),scored$TP==2L,scored$TN==1L,scored$FP==0L,
    scored$FN==0L,scored$Specificity==1,scored$BalancedAccuracy==1)
  refinement_args<-list(pattern="raw",x=c("raw_A","raw_B","raw_error","ratio_A"),
    truth=c(TRUE,TRUE,FALSE,FALSE),max_depth=3L,max_candidates=40L,frontier_limit=10L)
  refined_a<-do.call(refine_pattern_search,refinement_args)
  refined_b<-do.call(refine_pattern_search,refinement_args)
  stopifnot(identical(refined_a,refined_b),refined_a$status%in%c("reliable","provisional","unresolved","conflicting"),
    all(c("Action","Disposition","Reason")%in%names(refined_a$audit)),
    all(nzchar(refined_a$audit$Reason)),nrow(refined_a$candidates)<=40L)
  conflicting<-refine_pattern_search("same",c("same","same"),c(TRUE,FALSE),max_candidates=5L)
  stopifnot(identical(conflicting$status,"conflicting"),!is.null(conflicting$abstention))
  stopifnot(identical(extract_condition("pre_A_suf","between","pre_","_suf"),"A"))
  stopifnot(identical(extract_condition("A.mid.tail","start",after="\\."),"A"))
  stopifnot(identical(extract_condition("head.mid.A","end",before="\\."),"A"))
  stopifnot(identical(extract_condition("id12::A]","between","[[:alnum:]]+::","\\]"),"A"))
  stopifnot(identical(extract_condition(" A _ B ","phrase_position",separators="_",pos=1L)," A "))
  stopifnot(identical(extract_condition(c("run_A_x","run_B_x","run_A_y"),"pattern_detect",separators="_",pos=1L),c("A","B","A")))
  observed_separators<-condition_separators(c("A/B", "A\\B", "A  B", "A(B)"))
  stopifnot(all(c("\\/","\\\\","\\s+","\\(","\\)")%in%observed_separators),
    "\\(|\\)"%in%observed_separators,
    "\\)\\s+\\/\\s+\\("%in%condition_separators("Ratio (A) / (B)"))
  d<-data.frame(Column=c("raw_A","raw_B","ratio_A_B"),Content=c("Raw","Raw","Ratio"));z<-infer_content(d,0)
  # Transformation inference is label-scoped and independent of regex scoring.
  transformation_fixture<-data.frame(
    Column=c("raw_A","norm_A","pvalue_A","description","identifier","Row Index"),
    Content=c("Raw Abundance","Normalized Abundance","Abundance Ratio p-Value",
      "Description","Identifier","Row Index"),
    Transformation=c("log2","log10","-log10",NA_character_,NA_character_,NA_character_),
    stringsAsFactors=FALSE,check.names=FALSE)
  stopifnot(identical(infer_content_transformation(transformation_fixture,"Raw Abundance"),"log2"),
    identical(infer_content_transformation(transformation_fixture,"Normalized Abundance"),"log10"),
    identical(infer_content_transformation(transformation_fixture,"Abundance Ratio p-Value"),"-log10"),
    is.na(infer_content_transformation(transformation_fixture,"Description")),
    is.na(infer_content_transformation(transformation_fixture,"Identifier")),
    is.na(infer_content_transformation(transformation_fixture,"Row Index")))
  blank_transformation<-data.frame(Column="raw_blank",Content="Raw Abundance",
    Transformation="",stringsAsFactors=FALSE)
  stopifnot(identical(infer_content_transformation(blank_transformation,"Raw Abundance"),"None"))
  conflict_transformation<-data.frame(Column=c("raw_A","raw_B","description"),
    Content=c("Raw Abundance","Raw Abundance","Description"),Transformation=c("log2","log10",NA_character_),
    stringsAsFactors=FALSE)
  conflict_error<-tryCatch(infer_content_transformation(conflict_transformation,"Raw Abundance"),error=identity)
  conflict_details<-content_transformation_details(conflict_transformation,"Raw Abundance")
  conflict_fit<-infer_content(conflict_transformation,0L)
  stopifnot(inherits(conflict_error,"miraprot_transformation_ambiguity") ||
      inherits(conflict_error,"miraprot_transformation_conflict"),
    inherits(conflict_error,"miraprot_transformation_validation_error"),
    identical(conflict_details$source,"conflict"),
    identical(conflict_details$values,c("log2","log10")),
    identical(conflict_details$row_ids,1:2),
    "Raw Abundance"%in%conflict_fit$table$Content,
    is.na(conflict_fit$table$Transformation[conflict_fit$table$Content=="Raw Abundance"]))
  unsupported_transformation<-data.frame(Column="raw_bad",Content="Raw Abundance",
    Transformation="Log2",stringsAsFactors=FALSE)
  unsupported_error<-tryCatch(infer_content_transformation(unsupported_transformation,"Raw Abundance"),error=identity)
  unsupported_details<-content_transformation_details(unsupported_transformation,"Raw Abundance")
  stopifnot(inherits(unsupported_error,"miraprot_transformation_validation_error"),
    identical(unsupported_details$source,"unsupported"),
    any(grepl("unsupported",validate_metadata(unsupported_transformation)$Message,fixed=TRUE)))
  transformation_fit<-infer_content(transformation_fixture,0L)
  expected_rules<-c("Raw Abundance"="log2","Normalized Abundance"="log10",
    "Abundance Ratio p-Value"="-log10")
  stopifnot(all(vapply(names(expected_rules),function(label)
    transformation_fit$table$Transformation[transformation_fit$table$Content==label]==expected_rules[[label]],logical(1))))
  transformation_replay<-apply_content_table(transformation_fixture,transformation_fit$table)
  stopifnot(identical(transformation_replay$metadata$Transformation,transformation_fixture$Transformation),
    all(transformation_replay$rows$TransformationMatch),
    all(c("ExpectedTransformation","PredictedTransformation","TransformationMatch",
      "TransformationFailureReason")%in%names(transformation_replay$rows)))
  transformation_rules<-coerce_contract(list(table=transformation_fit$table,
    condition=empty_condition(),ratio=empty_ratio()))
  transformation_path<-tempfile(fileext=".rds");on.exit(unlink(transformation_path),add=TRUE)
  saveRDS(transformation_rules,transformation_path);transformation_reloaded<-readRDS(transformation_path)
  stopifnot(identical(transformation_reloaded$table$Transformation,
    transformation_rules$table$Transformation),!length(validate_export(transformation_reloaded)),
    identical(apply_content_table(transformation_fixture,transformation_reloaded$table)$metadata$Transformation,
      transformation_fixture$Transformation))
  supported_fixture<-list(table=data.frame(
    Content=c(rep("Raw Abundance",4L),"Description","Row Index"),
    Include=c("raw_none","raw_log2","raw_log10","raw_neglog10","description","Row Index"),
    Exclude=rep("",6L),Transformation=c(SUPPORTED_TRANSFORMATIONS,"", ""),
    stringsAsFactors=FALSE,check.names=FALSE),condition=empty_condition(),ratio=empty_ratio())
  supported_rules<-coerce_contract(supported_fixture)
  stopifnot(identical(supported_rules$table$Transformation,
      c(SUPPORTED_TRANSFORMATIONS,NA_character_,NA_character_)),
    is.character(supported_rules$table$Transformation))
  supported_path<-tempfile(fileext=".rds");on.exit(unlink(supported_path),add=TRUE)
  saveRDS(supported_rules,supported_path)
  stopifnot(identical(readRDS(supported_path)$table$Transformation,
    supported_rules$table$Transformation))
  manual_rules<-transformation_rules
  manual_at<-c(match("Raw Abundance",manual_rules$table$Content),
    match("Normalized Abundance",manual_rules$table$Content),
    match("Abundance Ratio p-Value",manual_rules$table$Content))
  for(i in seq_along(manual_at)) {
    manual_rules$table$Transformation[manual_at[i]]<-c("log2","log10","-log10")[i]
    retested<-coerce_contract(manual_rules)
    stopifnot(identical(retested$table$Transformation[manual_at[i]],
      c("log2","log10","-log10")[i]))
  }
  stopifnot(!length(validate_export(manual_rules)),
    all(apply_content_table(transformation_fixture,manual_rules$table)$rows$TransformationMatch))
  # Logging is observability-only: all three user debug levels must preserve
  # byte-for-byte rule tables and diagnostics.
  level_results<-lapply(c("info","debug","trace"),function(level)
    infer_content(d,0,create_session_logger(threshold=level)))
  stopifnot(all(vapply(level_results,function(value)identical(value$table,level_results[[1L]]$table),logical(1))),
    all(vapply(level_results,function(value)identical(value$metrics,level_results[[1L]]$metrics),logical(1))))
  stopifnot(identical(names(z$table),CONTENT_FIELDS));x<-coerce_contract(list(table=z$table,condition=empty_condition(),ratio=empty_ratio()));stopifnot(!length(validate_export(x)))
  stopifnot(length(validate_export(x[c("condition","table","ratio")])))
  too_complex<-x;too_complex$table<-data.frame(Content="Raw",Include=paste(rep("a+",MAX_REGEX_COMPLEXITY+1L),collapse=""),Exclude="",Transformation="",check.names=FALSE)
  stopifnot(any(grepl("complexity",validate_export(too_complex))))
  template<-roundtrip_export(x,data.frame(Column="raw_A"));stopifnot(identical(names(template)[1:4],c("table","condition","ratio","debug_info")),!length(validate_export(template)))
  # Export is an explicit, cached transaction.  Instrumented operations prove
  # that one request performs one normalization, validation, construction,
  # write, and read, while status/UI inspection performs none of them.
  export_counts<-new.env(parent=emptyenv());for(n in c("normalize","validate","construct","save","read"))export_counts[[n]]<-0L
  counted<-list(
    normalize=function(value){export_counts$normalize<-export_counts$normalize+1L;coerce_contract(value)},
    validate=function(value,logger=.miraprot_noop_logger){export_counts$validate<-export_counts$validate+1L;validate_export(value,logger=logger)},
    construct=function(value,logger=.miraprot_noop_logger,rules_are_normalized=FALSE){export_counts$construct<-export_counts$construct+1L;build_export_template(value,logger=logger,rules_are_normalized=rules_are_normalized)},
    save=function(value,path){export_counts$save<-export_counts$save+1L;saveRDS(value,path)},
    read=function(path){export_counts$read<-export_counts$read+1L;readRDS(path)})
  preparation_log<-create_session_logger(threshold="info")
  prepared<-prepare_export_artifact(x,data.frame(Column="raw_A"),
    list(unreliable_action="exclude",unresolved_action="exclude"),
    preparation_log,counted)
  operation_names<-c("normalize","validate","construct","save","read")
  stopifnot(!length(prepared$errors),!is.null(prepared$artifact),
    identical(unname(vapply(operation_names,function(n)export_counts[[n]],integer(1))),rep(1L,5L)),
    length(attr(preparation_log,"records")())==2L)
  before_counts<-vapply(operation_names,function(n)export_counts[[n]],integer(1));status_text<-if(length(prepared$errors))"failed"else"ready"
  invisible(rep(status_text,3L));invisible(as.character(build_ui()))
  stopifnot(identical(vapply(operation_names,function(n)export_counts[[n]],integer(1)),before_counts),
    length(attr(preparation_log,"records")())==2L,
    grepl('id="prepare_export"',as.character(build_ui()),fixed=TRUE),
    grepl('id="download_rds"',as.character(build_ui()),fixed=TRUE),
    grepl('id="download_xlsx"',as.character(build_ui()),fixed=TRUE),
    !grepl('download_controls',as.character(build_ui()),fixed=TRUE))
  changed_rules<-x;changed_rules$table$Include[1L]<-paste0(changed_rules$table$Include[1L],"$")
  changed_signature<-export_signature(changed_rules,data.frame(Column="raw_A"),
    list(unreliable_action="exclude",unresolved_action="exclude"))
  stopifnot(!identical(changed_signature,prepared$signature))
  unresolved_metadata<-data.frame(Column=c("raw_A","unknown"))
  exclude_settings<-list(unreliable_action="exclude",unresolved_action="exclude")
  include_settings<-list(unreliable_action="exclude",unresolved_action="include")
  excluded_metadata<-prepare_export_inputs(x,NULL,unresolved_metadata,"exclude","exclude")$metadata
  stopifnot(nrow(excluded_metadata)==1L,identical(excluded_metadata$Column,"raw_A"),
    !identical(export_signature(x,excluded_metadata,exclude_settings),
      export_signature(x,unresolved_metadata,include_settings)))
  inclusion_log<-create_session_logger(threshold="info")
  included<-prepare_export_artifact(x,unresolved_metadata,include_settings,inclusion_log)
  stopifnot(!length(included$errors),included$counts$metadata$included==2L,
    included$counts$metadata$excluded_unresolved==0L)
  # Every policy choice changes either persisted rule rows or replay rows.  The
  # status frame is authoritative; warning/display strings are not consulted.
  policy_rules<-x
  policy_rules$table<-rbind(policy_rules$table,
    data.frame(Content="Questionable",Include="unknown",Exclude="",Transformation="None"))
  policy_status<-list(rule_status=list(table=data.frame(Content=c("Raw","Questionable"),
    Status=c("reliable","provisional"),stringsAsFactors=FALSE)))
  excluded_rules<-prepare_export_inputs(policy_rules,policy_status,unresolved_metadata,"exclude","include")
  included_rules<-prepare_export_inputs(policy_rules,policy_status,unresolved_metadata,"include","include")
  stopifnot(!"Questionable"%in%excluded_rules$table$Content,
    "Questionable"%in%included_rules$table$Content,
    "Row Index"%in%excluded_rules$table$Content,
    excluded_rules$counts$rules$excluded_unreliable[["table"]]==1L)
  excluded_artifact<-prepare_export_artifact(policy_rules,unresolved_metadata,
    list(unreliable_action="exclude",unresolved_action="include"),metrics=policy_status)
  included_artifact<-prepare_export_artifact(policy_rules,unresolved_metadata,
    list(unreliable_action="include",unresolved_action="include"),metrics=policy_status)
  stopifnot(identical(excluded_artifact$artifact$table,excluded_rules$table),
    identical(included_artifact$artifact$table,included_rules$table))
  excluded_rows<-prepare_export_inputs(x,NULL,unresolved_metadata,"include","exclude")
  included_rows<-prepare_export_inputs(x,NULL,unresolved_metadata,"include","include")
  stopifnot(nrow(excluded_rows$metadata)==1L,nrow(excluded_rows$replay_rows)==1L,
    nrow(included_rows$metadata)==2L,nrow(included_rows$replay_rows)==2L,
    !identical(excluded_rows$signature_payload,included_rows$signature_payload))
  stale_path<-tempfile(fileext=".rds")
  stale_error<-tryCatch({write_cached_rds(prepared$artifact,changed_signature,prepared$signature,stale_path);NULL},error=conditionMessage)
  stopifnot(grepl("stale",stale_error,fixed=TRUE),!file.exists(stale_path))
  valid_path<-tempfile(fileext=".rds");write_cached_rds(prepared$artifact,prepared$signature,prepared$signature,valid_path)
  stopifnot(file.exists(valid_path),identical(readRDS(valid_path),prepared$artifact));unlink(valid_path)
  # The status renderer reads cached fields only.  Re-rendering it neither logs
  # nor loops when a logger bump changes log_version.
  shiny::testServer(server,{
    records<-session$userData$miraprot_log_records
    initial<-length(records());invisible(output$export_status);invisible(output$export_status);session$flushReact()
    stopifnot(length(records())==initial)
    state<-session$userData$miraprot_export_state
    state$rules<-x;session$flushReact();state$export_artifact<-prepared$artifact
    state$export_signature<-prepared$signature;state$export_status<-"ready"
    state$rules<-changed_rules;session$flushReact()
    stopifnot(is.null(state$export_artifact),identical(state$export_status,"not_prepared"))
    logger_for_session<-session$userData$miraprot_logger
    logger_for_session("info","export","roundtrip","Loop guard probe.")
    after_log<-length(records());invisible(output$export_status);invisible(output$export_status);session$flushReact()
    stopifnot(length(records())==after_log)
  })
  # Cross-rule one-row matrix.  Each applicable family must accept and emit a
  # rule from one complete row; a blank condition reference is non-applicable,
  # while genuinely contradictory source evidence remains unresolved.
  matrix_content_fixture<-data.frame(Column="Unique Raw A",Content="Raw",
    stringsAsFactors=FALSE)
  matrix_condition_fixture<-data.frame(Column="pre_A_suf",Content="Raw Abundance",
    Options="A",stringsAsFactors=FALSE)
  matrix_ratio_fixture<-data.frame(Column="Abundance Ratio A/B",
    Content="Abundance Ratio",Options="A",Numerator="A",Denominator="B",
    stringsAsFactors=FALSE)
  matrix_blank_fixture<-data.frame(Column="Raw",Content="Raw",
    Options="",stringsAsFactors=FALSE)
  matrix_conflict_fixture<-data.frame(Column=c("identical source","identical source"),
    Content=c("First","Second"),stringsAsFactors=FALSE)
  matrix_content<-infer_content(matrix_content_fixture,0L)
  matrix_condition<-infer_conditions(matrix_condition_fixture,"Options")
  matrix_ratio<-infer_ratios(matrix_ratio_fixture)
  matrix_blank<-infer_conditions(matrix_blank_fixture,"Options")
  matrix_conflict<-infer_content(matrix_conflict_fixture,0L)
  cross_rule_matrix<-data.frame(
    RuleFamily=c("Content","Condition","Ratio","Condition","Any"),
    ValidRow=c("one complete row","populated target","numerator/denominator pair",
      "blank target","conflicting identical source"),
    ExpectedResult=c("Reliable and exported","Reliable and exported",
      "Reliable and exported","Not applicable","Unresolved"),
    ActualResult=c(
      if("Raw"%in%matrix_content$table$Content&&matrix_content$status$Status=="reliable") "Reliable and exported" else "Unresolved",
      if(nrow(matrix_condition$table)==1L&&matrix_condition$status$Status=="reliable") "Reliable and exported" else "Unresolved",
      if(nrow(matrix_ratio$table)==1L&&matrix_ratio$status$Status=="reliable") "Reliable and exported" else "Unresolved",
      if(!nrow(matrix_blank$table)&&matrix_blank$status$Status=="not_applicable") "Not applicable" else "Unresolved",
      if(!any(matrix_conflict$status$Reliable)&&all(matrix_conflict$status$Status=="unresolved")) "Unresolved" else "Reliable and exported"),
    stringsAsFactors=FALSE)
  stopifnot(identical(cross_rule_matrix$ActualResult,cross_rule_matrix$ExpectedResult),
    matrix_ratio$status$SuccessfulRows==1L,
    matrix_ratio$status$ApplicableRows==1L,
    matrix_blank$status$UnavailableReferences==1L,
    !nrow(matrix_blank$unresolved_reasons),
    !any(grepl("condition extraction unresolved",matrix_blank$warnings,fixed=TRUE)))

  # Golden workbook regression.  This is deliberately read through readxl (the
  # same boundary used by the interactive upload path) rather than reconstructed
  # as an in-memory frame.  The checked-in expectations are predicates over the
  # complete returned objects, so changes in Excel typing, applicability, rule
  # status, or replay behavior cannot silently redefine the migration baseline.
  golden_fixture_dir<-file.path(dirname(source_path),"tests","fixtures",
    "regex_metadata_assistant")
  golden_sheet_names<-c("content","condition","ratio")
  golden_sheet_sources<-setNames(lapply(golden_sheet_names,function(sheet) {
    path<-file.path(golden_fixture_dir,paste0(sheet,".csv"))
    stopifnot(file.exists(path))
    read.csv(path,check.names=FALSE,na.strings="",stringsAsFactors=FALSE)
  }),golden_sheet_names)
  golden_workbook<-tempfile(fileext=".xlsx")
  on.exit(unlink(golden_workbook),add=TRUE)
  openxlsx::write.xlsx(golden_sheet_sources,golden_workbook,overwrite=TRUE)
  stopifnot(identical(readxl::excel_sheets(golden_workbook),golden_sheet_names))
  golden_content<-as.data.frame(readxl::read_excel(golden_workbook,sheet="content",
    .name_repair="minimal"),check.names=FALSE)
  golden_condition<-as.data.frame(readxl::read_excel(golden_workbook,sheet="condition",
    .name_repair="minimal"),check.names=FALSE)
  golden_ratio<-as.data.frame(readxl::read_excel(golden_workbook,sheet="ratio",
    .name_repair="minimal"),check.names=FALSE)
  golden_content_result<-infer_content(golden_content,0L)
  golden_condition_result<-infer_conditions(golden_condition,"Options")
  golden_ratio_result<-infer_ratios(golden_ratio)
  golden_transforms<-setNames(golden_content_result$table$Transformation,
    golden_content_result$table$Content)
  stopifnot(
    identical(unname(golden_transforms[c("Raw Abundance","Normalized Abundance",
      "Abundance Ratio p-Value")]),c("log2","log10","-log10")),
    all(c("Description","Identifier","Row Index")%in%golden_content_result$table$Content),
    nrow(golden_condition_result$table)==1L,
    identical(golden_condition_result$table$Content,"Raw Abundance"),
    identical(golden_condition_result$status$Status[golden_condition_result$status$Content=="Raw Abundance"],"reliable"),
    all(golden_condition_result$status$Status[golden_condition_result$status$Content%in%
      c("Description","Identifier","Row Index","# PSMs")]=="not_applicable"),
    identical(apply_condition_table(golden_condition,golden_condition_result$table)$metadata$Options,
      golden_condition$Options),
    nrow(golden_ratio_result$table)==1L,
    identical(golden_ratio_result$status$Status,"reliable"),
    golden_ratio_result$status$SuccessfulRows==1L,
    golden_ratio_result$status$ApplicableRows==1L)
  q<-infer_conditions(data.frame(Column=c("pre_A_suf","pre_B_suf","pre_C_suf"),Content="Raw Abundance",Options=c("A","B","C")),"Options")
  stopifnot(nrow(q$table)==1L,nrow(q$diagnostics)<=3L*34L,all(q$diagnostics$CandidateRank>=1L),
    q$status$Status=="reliable",all(c("IncorrectNonemptyResults","EmptyResults","Ambiguities","ConstantOutputFailure")%in%names(q$diagnostics)),
    all(apply_condition_table(data.frame(Column=c("pre_A_suf","pre_B_suf","pre_C_suf"),Content="Raw Abundance"),q$table,c("A","B","C"))$diagnostics$ExactMatch))
  # Snapshot an already-successful sample-bearing group before exercising the
  # semantic exclusion. Adding rows with complete (and deliberately
  # extractable) references must not perturb its selected rule or replay.
  working_condition_fixture<-data.frame(
    Column=c("pre_A_suf","pre_B_suf","pre_C_suf"),Content="Raw Abundance",
    Options=c("A","B","C"),stringsAsFactors=FALSE)
  working_condition_snapshot<-infer_conditions(working_condition_fixture,"Options")
  working_with_non_sample<-rbind(working_condition_fixture,data.frame(
    Column=c("pre_Description_suf","pre_Identifier_suf","pre_Index_suf","pre_PSM_suf"),
    Content=c("Description","Identifier","Row Index","# PSMs"),
    Options=c("Description","Identifier","Index","PSM"),stringsAsFactors=FALSE))
  working_after_filter<-infer_conditions(working_with_non_sample,"Options")
  stopifnot(identical(
      working_after_filter$table[working_after_filter$table$Content=="Raw Abundance",,drop=FALSE],
      working_condition_snapshot$table),
    identical(
      working_after_filter$diagnostics[working_after_filter$diagnostics$Content=="Raw Abundance",,drop=FALSE],
      working_condition_snapshot$diagnostics),
    identical(
      apply_condition_table(working_condition_fixture,working_after_filter$table)$metadata$Options,
      c("A","B","C")))

  # Mixed real-world metadata regression. Every row has a reference embedded in
  # Column, including all excluded content types, proving non-applicability is
  # decided from Content semantics rather than from a missing target.
  mixed_non_sample_labels<-c("Description","Identifier","Row Index","# PSMs",
    "Abundance Ratio","Abundance Ratio p-Value",
    "Abundance Ratio Adj. p-Value","Abundance Ratio q-Value")
  mixed_abundance_labels<-c("Abundance Ratio","Abundance Ratio p-Value",
    "Abundance Ratio Adj. p-Value","Abundance Ratio q-Value")
  mixed_labels<-c(SAMPLE_BEARING_CONTENT_TYPES,mixed_non_sample_labels)
  mixed_samples<-paste0("Sample",seq_along(SAMPLE_BEARING_CONTENT_TYPES))
  mixed_metadata_fixture<-data.frame(
    Column=c(paste0(SAMPLE_BEARING_CONTENT_TYPES," [",mixed_samples,"]"),
      "Description [DescRef]","Identifier [IdRef]","Row Index [IndexRef]",
      "# PSMs [PsmRef]","Abundance Ratio: (Case) / (Control)",
      "Abundance Ratio p-Value: (Case) / (Control)",
      "Abundance Ratio Adj. p-Value: (Case) / (Control)",
      "Abundance Ratio q-Value: (Case) / (Control)"),
    Content=mixed_labels,
    Options=c(mixed_samples,"DescRef","IdRef","IndexRef","PsmRef",rep("Ratio",4L)),
    Numerator=c(rep("",length(SAMPLE_BEARING_CONTENT_TYPES)+4L),rep("Case",4L)),
    Denominator=c(rep("",length(SAMPLE_BEARING_CONTENT_TYPES)+4L),rep("Control",4L)),
    stringsAsFactors=FALSE)
  mixed_content_before<-infer_content(mixed_metadata_fixture,0L)
  mixed_ratio_before<-infer_ratios(mixed_metadata_fixture)
  mixed_conditions<-infer_conditions(mixed_metadata_fixture,"Options")
  mixed_content_after<-infer_content(mixed_metadata_fixture,0L)
  mixed_ratio_after<-infer_ratios(mixed_metadata_fixture)
  mixed_retest<-apply_condition_table(mixed_metadata_fixture,mixed_conditions$table)
  excluded_condition_labels<-mixed_non_sample_labels
  retained_condition_labels<-SAMPLE_BEARING_CONTENT_TYPES
  stopifnot(
    !any(excluded_condition_labels%in%mixed_conditions$table$Content),
    all(retained_condition_labels%in%mixed_conditions$table$Content),
    all(mixed_conditions$status$Status[mixed_conditions$status$Content%in%excluded_condition_labels]=="not_applicable"),
    !nrow(mixed_conditions$unresolved_reasons),
    !length(mixed_conditions$warnings),
    all(mixed_retest$metadata$Options[mixed_retest$metadata$Content%in%excluded_condition_labels]
      %in% c("","Ratio")),
    identical(mixed_retest$metadata$Options[mixed_retest$metadata$Content%in%retained_condition_labels],
      mixed_samples),
    identical(mixed_retest$metadata$Options[mixed_retest$metadata$Content%in%AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT],
      rep("Ratio",3L)),
    mixed_retest$metadata$Options[mixed_retest$metadata$Content=="Abundance Ratio q-Value"]=="",
    identical(mixed_content_after,mixed_content_before),
    identical(mixed_ratio_after,mixed_ratio_before),
    identical(mixed_ratio_after$table$Content,sort(mixed_abundance_labels)),
    all(mixed_ratio_after$status$Status=="reliable"))
  mixed_rules<-coerce_contract(list(table=mixed_content_after$table,
    condition=mixed_conditions$table,ratio=mixed_ratio_after$table))
  mixed_roundtrip<-roundtrip_export(mixed_rules,mixed_metadata_fixture)
  stopifnot(identical(names(mixed_rules),c("table","condition","ratio")),
    identical(names(mixed_rules$table),CONTENT_FIELDS),
    identical(names(mixed_rules$condition),CONDITION_FIELDS),
    identical(names(mixed_rules$ratio),RATIO_FIELDS),
    !length(validate_export(mixed_rules)),!length(validate_export(mixed_roundtrip)),
    identical(mixed_roundtrip$table,mixed_rules$table),
    identical(mixed_roundtrip$condition,mixed_rules$condition),
    identical(mixed_roundtrip$ratio,mixed_rules$ratio))
  # A single annotated row is complete evidence, while an absent annotation is
  # an applicability fact rather than a failed rule.  Mixed groups retain that
  # distinction in row diagnostics without allowing the blank to become a
  # successful extraction target.
  one_condition<-infer_conditions(data.frame(Column="pre_A_suf",Content="Raw Abundance",Options="A"),"Options")
  missing_condition<-infer_conditions(data.frame(Column="raw",Content="Raw Abundance",Options=""),"Options")
  incorrect_condition<-infer_conditions(data.frame(Column="raw",Content="Raw Abundance",Options="A"),"Options")
  constant_condition<-infer_conditions(data.frame(Column="A",Content="# PSMs",Options="A"),"Options")
  mixed_condition<-infer_conditions(data.frame(Column=c("row_A","row_without_reference"),Content="Row Index",Options=c("A","")),"Options")
  stopifnot(nrow(one_condition$table)==1L,one_condition$status$Status=="reliable",
    !one_condition$status$ConstantOutputFailure,
    missing_condition$status$Status=="not_applicable",!nrow(missing_condition$table),
    grepl("inference was skipped",missing_condition$warnings,fixed=TRUE),
    !nrow(missing_condition$unresolved_reasons),
    incorrect_condition$status$Status=="unresolved",!nrow(incorrect_condition$table),
    !nrow(constant_condition$table),constant_condition$status$Status=="not_applicable",
    !nrow(mixed_condition$table),mixed_condition$status$Status=="not_applicable",
    mixed_condition$status$UnavailableReferences==1L,
    !length(constant_condition$warnings), !length(mixed_condition$warnings))
  # Assistant semantic validation is deliberately limited to observed labels.
  # Invalid rows are diagnosed, ignored during replay, and removed from export
  # preparation without changing the persisted condition-table schema.
  invalid_labels<-c("Identifier","Description","Row Index","# PSMs")
  stale_conditions<-data.frame(Content=invalid_labels,Method="whole",Before="",After="",Separators="",Pos=1L,check.names=FALSE)
  stale_rules<-x;stale_rules$condition<-stale_conditions
  stale_metadata<-data.frame(Column=invalid_labels,Content=invalid_labels,Options=paste0("stale",seq_along(invalid_labels)))
  stale_applied<-apply_condition_table(stale_metadata,stale_conditions)
  stale_prepared<-prepare_export_inputs(stale_rules,NULL,stale_metadata,"include","include")
  stale_messages<-validate_export(stale_rules)
  stopifnot(length(stale_messages)==length(invalid_labels),
    all(vapply(invalid_labels,function(label)any(grepl(label,stale_messages,fixed=TRUE)),logical(1))),
    !nrow(stale_prepared$condition),identical(names(stale_prepared$condition),CONDITION_FIELDS),
    all(stale_applied$metadata$Options==""),!nrow(stale_applied$failures),
    all(!stale_applied$diagnostics$ExtractionFailure),
    !is_sample_bearing_content("Unobserved Repository Sample Type"))
  applied_content<-apply_content_table(data.frame(Column=c("raw_A","ratio_A_B"),Content=c("Raw","Ratio")),data.frame(Content=c("Raw","Ratio"),Include=c("^raw","ratio"),Exclude=c("",""),Transformation=c("None","log2"),check.names=FALSE))
  stopifnot(identical(applied_content$metadata$Content,c("Raw","Ratio")),!any(applied_content$rows$Conflict),sum(applied_content$metrics$FalsePositives)==0L)
  applied_content$metadata$Content[1L] <- "Raw Abundance"
  applied_condition<-apply_condition_table(applied_content$metadata,data.frame(Content="Raw Abundance",Method="end",Before="_",After="",Separators="",Pos=1L,check.names=FALSE),c("A",""))
  stopifnot(applied_condition$metadata$Options[1]=="A")
  retained_condition<-apply_condition_table(
    data.frame(Column=c("raw_A","ratio_A_B"),Content=c("Raw Abundance","Ratio"),Options=c("verified","A")),
    data.frame(Content="Raw Abundance",Method="end",Before="_",After="",Separators="",Pos=1L,check.names=FALSE))
  stopifnot(identical(retained_condition$metadata$Options,c("A","")),
    identical(known_samples_after_conditions(retained_condition$metadata),"A"))
  rr<-data.frame(Content="Ratio",Method="Position in String",Separators="_",Invert=FALSE,NumBefore="",NumAfter="",DenBefore="",DenAfter="",NumPos=2L,DenPos=3L,check.names=FALSE)
  applied_ratio<-apply_ratio_table(applied_condition$metadata,rr,c("","A"),c("","B"));stopifnot(applied_ratio$diagnostics$Success[2])
  ratio_examples<-data.frame(
    Column=c("Ratio A/B","Ratio A / B","Ratio (A)/(B)","B_A","B / A"),
    Content=c(rep("Ratio",3L),rep("Reversed Ratio",2L)),Options=c("A","B","A","B","A"),
    Numerator=rep("A",5L),Denominator=rep("B",5L),
    stringsAsFactors=FALSE)
  inferred_ratio<-infer_ratios(ratio_examples)
  selected_ratio_diag<-inferred_ratio$diagnostics[inferred_ratio$diagnostics$Selected,,drop=FALSE]
  stopifnot(nrow(inferred_ratio$table)==2L,
    all(inferred_ratio$status$Status=="reliable"),
    all(unique(inferred_ratio$diagnostics$Method)%in%RATIO_METHODS),
    all(selected_ratio_diag$PredictedNumerator=="A"),
    all(selected_ratio_diag$PredictedDenominator=="B"),
    all(selected_ratio_diag$Success),all(selected_ratio_diag$ReplaySuccess),
    all(selected_ratio_diag$ApplicableContentTargeted),
    all(selected_ratio_diag$KnownSampleCount==length(known_samples_after_conditions(
      apply_condition_table(ratio_examples,infer_conditions(ratio_examples,"Options")$table)$metadata))))
  # One exact applicable row is sufficient evidence for each representative
  # ratio label.  Exercise direct and serialized application independently so
  # neither inference-only scoring nor in-memory rule shape can mask a failure.
  abundance_labels<-c("Abundance Ratio","Abundance Ratio p-Value",
    "Abundance Ratio Adj. p-Value")
  # Real export shapes use parenthesized comparisons following a colon.  Ratio
  # Options are populated by content assignment, so all three condition groups
  # are explicitly non-applicable rather than misleading 0% failures.
  abundance_condition_fixture<-data.frame(
    Column=c("Abundance Ratio: (Case) / (Control)",
      "Abundance Ratio p-Value: (Case) / (Control)",
      "Abundance Ratio Adj. p-Value: (Case) / (Control)"),
    Content=abundance_labels,Options=rep("Ratio",3L),stringsAsFactors=FALSE)
  abundance_conditions<-infer_conditions(abundance_condition_fixture,"Options")
  stopifnot(!nrow(abundance_conditions$table),
    all(abundance_conditions$status$Status=="not_applicable"),
    !nrow(abundance_conditions$diagnostics),
    !nrow(abundance_conditions$unresolved_reasons),
    !length(abundance_conditions$warnings),
    identical(apply_condition_table(abundance_condition_fixture,
      abundance_conditions$table)$metadata$Options,rep("Ratio",3L)))
  for(label in abundance_labels) {
    fixture<-data.frame(Column=paste(label,"A/B"),Content=label,Options="A",
      Numerator="A",Denominator="B",stringsAsFactors=FALSE)
    fit<-infer_ratios(fixture)
    selected<-fit$diagnostics[fit$diagnostics$Selected,,drop=FALSE]
    direct<-apply_ratio_table(fixture,fit$table,"A","B")
    rules<-x;rules$ratio<-fit$table
    path<-tempfile(fileext=".rds");saveRDS(fit$table,path)
    reloaded<-readRDS(path);unlink(path)
    serialized<-apply_ratio_table(fixture,reloaded,"A","B")
    stopifnot(sum(fit$diagnostics$Applicable)==1L*nrow(fit$diagnostics),
      any(fit$diagnostics$Success),nrow(fit$table)==1L,nrow(selected)==1L,
      nrow(fit$diagnostics)<=12L,
      fit$status$Status=="reliable",fit$status$SuccessfulRows==1L,
      fit$status$ApplicableRows==1L,nzchar(fit$status$SelectedMethod),
      direct$diagnostics$Success,direct$metadata$Numerator=="A",
      direct$metadata$Denominator=="B",serialized$diagnostics$Success,
      serialized$metadata$Numerator=="A",serialized$metadata$Denominator=="B",
      selected$ReplaySuccess,selected$ReplayNumerator=="A",
      selected$ReplayDenominator=="B",!length(validate_export(rules)),
      identical(reloaded,fit$table))
  }
  abundance_fixture<-data.frame(
    Column=paste(abundance_labels,"A/B"),Content=abundance_labels,Options="A",
    Numerator="A",Denominator="B",stringsAsFactors=FALSE)
  abundance_content<-infer_content(abundance_fixture,0L)
  abundance_targeted<-apply_content_table(abundance_fixture,abundance_content$table)
  # Replay inference from the original mapped rows so this exercises content
  # assignment and ratio application as one complete pipeline.
  abundance_ratios<-infer_ratios(abundance_fixture)
  abundance_replay<-apply_ratio_table(abundance_targeted$metadata,
    abundance_ratios$table,"A","B")
  stopifnot(all(abundance_labels%in%abundance_content$table$Content),
    identical(abundance_targeted$metadata$Content,abundance_labels),
    identical(abundance_ratios$table$Content,sort(abundance_labels)),
    all(abundance_ratios$status$RatioRuleStatus=="reliable"),
    all(abundance_ratios$status$PipelineReachabilityStatus=="reached"),
    all(abundance_ratios$status$PipelineReachabilityReason==
      "Content assignment targeted every applicable reference row."),
    all(abundance_ratios$diagnostics[abundance_ratios$diagnostics$Selected,"ReplaySuccess"]),
    all(abundance_ratios$diagnostics[abundance_ratios$diagnostics$Selected,"ApplicableContentTargeted"]),
    all(abundance_replay$diagnostics$Applicable),
    all(abundance_replay$diagnostics$Success),
    identical(abundance_replay$metadata$Numerator,rep("A",3L)),
    identical(abundance_replay$metadata$Denominator,rep("B",3L)))

  # Pattern Recognition can fit the reference pair using a local dictionary
  # yet fail when that dictionary is absent from effective application context.
  # A complete position candidate still makes inference resolvable.
  fallback_fixture<-data.frame(Column="Abundance Ratio A/B",Content="Abundance Ratio",
    Options="",Numerator="A",Denominator="B",stringsAsFactors=FALSE)
  recognition_candidate<-data.frame(Content="Abundance Ratio",
    Method="Pattern Recognition",Separators="\\s+|/",Invert=FALSE,
    NumBefore=NA_character_,NumAfter=NA_character_,DenBefore=NA_character_,
    DenAfter=NA_character_,NumPos=NA_integer_,DenPos=NA_integer_,check.names=FALSE)
  local_recognition<-ratio_diagnostics("Abundance Ratio",1L,
    fallback_fixture$Column,"A","B",recognition_candidate,c("A","B"))
  contextual_recognition<-apply_ratio_table(fallback_fixture,
    recognition_candidate,"A","B")
  fallback_fit<-infer_ratios(fallback_fixture)
  stopifnot(local_recognition$Success,!contextual_recognition$diagnostics$Success,
    nrow(fallback_fit$table)==1L,
    fallback_fit$table$Method=="Position in String",
    fallback_fit$status$Status=="reliable")

  # No method may be exported when the source contains neither reference
  # component; incomplete and incorrect extractions remain unresolved.
  no_components<-data.frame(Column="Abundance Ratio",Content="Abundance Ratio",
    Options="",Numerator="A",Denominator="B",stringsAsFactors=FALSE)
  negative_fit<-infer_ratios(no_components)
  stopifnot(!nrow(negative_fit$table),negative_fit$status$Status=="unresolved",
    !any(negative_fit$diagnostics$Success))
  # Retention and display are independent: all three details are captured while
  # Summary emission/display is selected, and historical visibility is purely a
  # reversible view over the bounded records.  The complete-download frame is
  # deliberately built from retained records rather than the filtered view.
  old_recorder<-get0(".miraprot_log_record",envir=.GlobalEnv,inherits=FALSE)
  assign(".miraprot_log_record",function(...)invisible(TRUE),envir=.GlobalEnv)
  on.exit({if(is.null(old_recorder))rm(".miraprot_log_record",envir=.GlobalEnv)else assign(".miraprot_log_record",old_recorder,envir=.GlobalEnv)},add=TRUE)
  level_logger<-create_session_logger(threshold="info")
  level_logger("info",message="level-0")
  level_logger("debug",message="level-1")
  level_logger("trace",message="level-2")
  retained_levels<-attr(level_logger,"records")()
  visible_messages<-function(level)vapply(filter_session_log_entries(retained_levels,level),`[[`,character(1),"message")
  stopifnot(identical(visible_messages(0L),"level-0"),
    identical(visible_messages(1L),c("level-0","level-1")),
    identical(visible_messages(2L),c("level-0","level-1","level-2")),
    identical(visible_messages(0L),"level-0"),length(retained_levels)==3L,
    identical(session_log_frame(retained_levels)$Level,c("info","debug","trace")))
  complete_log_file<-tempfile(fileext=".csv")
  write.csv(session_log_frame(retained_levels),complete_log_file,row.names=FALSE,na="")
  downloaded_levels<-read.csv(complete_log_file,stringsAsFactors=FALSE)$Level
  unlink(complete_log_file)
  stopifnot(identical(downloaded_levels,c("info","debug","trace")))
  csv_messages<-c("comma, retained", "quote \"retained\"", "first line\nsecond line",
    "period. retained", "backslash \\\\ retained")
  csv_logger<-create_session_logger(threshold="error")
  for(message in csv_messages)csv_logger("info",message=message)
  csv_file<-tempfile(fileext=".csv")
  write.csv(session_log_frame(attr(csv_logger,"records")()),csv_file,row.names=FALSE,na="")
  csv_roundtrip<-read.csv(csv_file,stringsAsFactors=FALSE,check.names=FALSE)$Message
  unlink(csv_file)
  stopifnot(identical(csv_roundtrip,csv_messages))
  attr(level_logger,"clear")()
  stopifnot(!length(attr(level_logger,"records")()))

  # Hidden verbose history participates in the same oldest-first eviction and
  # clear does not write a replacement record into the newly emptied buffer.
  test_logger<-create_session_logger(threshold="info")
  for(i in seq_len(MIRAPROT_MAX_LOG_ENTRIES+1L))test_logger("trace",message=as.character(i))
  records<-attr(test_logger,"records")();stats<-attr(test_logger,"stats")()
  stopifnot(length(records)==MIRAPROT_MAX_LOG_ENTRIES,records[[1L]]$message=="2",stats$dropped==1L,
    !length(filter_session_log_entries(records,0L)),length(filter_session_log_entries(records,2L))==MIRAPROT_MAX_LOG_ENTRIES)
  attr(test_logger,"clear")();stopifnot(!length(attr(test_logger,"records")()),attr(test_logger,"stats")()$dropped==1L)

  # Golden/adversarial pure-function matrix.  These source strings deliberately
  # share shapes and delimiters across labels so success cannot come from
  # memorizing punctuation or an identifier class alone.
  golden <- c("Intensity SampleA (R1)", "Intensity SampleB (R2)",
              "Abundance SampleA (R1)", "Abundance SampleB (R2)")
  golden_labels <- c("Intensity", "Intensity", "Abundance", "Abundance")
  golden_fit <- infer_content(data.frame(Column=golden, Content=golden_labels), 0L)
  stopifnot(all(golden_fit$status$Status == "reliable"),
    identical(apply_content_table(data.frame(Column=golden), golden_fit$table)$metadata$Content,
      golden_labels))
  # Generalization, redundancy, and anchors are separate transformations.
  general <- candidate_fragments(golden[1:2], golden[3:4])
  stopifnot(length(general) <= CANDIDATE_FRAGMENT_SEARCH_LIMIT,
    any(grepl("[[:alpha:]]", general, fixed=TRUE)),
    identical(infer_anchors("Intensity", golden[1:2], golden[3:4]), "^Intensity"))
  no_redundancy <- infer_content(data.frame(Column=golden,Content=golden_labels),0L)
  with_redundancy <- infer_content(data.frame(Column=golden,Content=golden_labels),3L)
  stopifnot(identical(no_redundancy$status,with_redundancy$status),
    all(with_redundancy$redundancy_history$DeltaRecall == 0L))

  # Every supported punctuation/special token is reconstructed at its original
  # Unicode character position, and every literal survives a case-sensitive
  # anchored match.  Slash conversion applies only to persisted boundary fields.
  all_special <- paste0(c(names(TOKEN_PUNCTUATION),names(TOKEN_DELIMITERS)),collapse="")
  special_tokens <- tokens(paste0("Å",all_special,"β"))
  stopifnot(paste0(special_tokens$Text,collapse="") == paste0("Å",all_special,"β"),
    all(special_tokens$End-special_tokens$Start+1L == nchar(special_tokens$Text)),
    safe_grepl(paste0("^",regex_escape_literal(all_special),"$"),all_special),
    safe_grepl("^[[:alpha:]]+$",c("Åβ")), !safe_grepl("^[A-Z]+$","Åβ"))
  slash_fixtures <- c("/", "\\", "a/b", "a\\b", "a\\/b", "\\\\/", "x/y\\z")
  for(field in c("content","condition_boundary","ratio_boundary")) {
    persisted <- regex_to_miraprot_storage(slash_fixtures,field)
    stopifnot(identical(regex_from_miraprot_storage(persisted,field),slash_fixtures),
      identical(regex_to_miraprot_storage(persisted,field),persisted))
  }
  stopifnot(identical(regex_to_miraprot_storage(slash_fixtures,"separator"),slash_fixtures),
    safe_grepl("^a\\s+b$","a  b"), !safe_grepl("^a\\s+b$","ab"),
    safe_grepl("^a\\s*b$",c("ab","a  b")))
  unicode_pattern <- "^Καλημέρα\\s+世界$"
  stopifnot(validate_pcre(unicode_pattern)$valid,
    validate_stringr_pattern(unicode_pattern)$valid,
    safe_grepl(unicode_pattern,"Καλημέρα 世界"),
    !safe_grepl(unicode_pattern,"καλημέρα 世界"))

  # All condition executors and all ratio executors are checked directly,
  # independent of whether inference happens to select that family.
  stopifnot(identical(extract_condition("A","whole"),"A"),
    identical(extract_condition("A_tail","start",after="_"),"A"),
    identical(extract_condition("head_A","end",before="_"),"A"),
    identical(extract_condition("head_A_tail","between","head_","_tail"),"A"),
    identical(extract_condition("x_A_y","phrase_position",separators="_",pos=2L),"A"),
    identical(extract_condition(c("x_A","x_B"),"pattern_detect",separators="_",pos=1L),c("A","B")))
  regex_ratio <- data.frame(Method="Regular Expressions",Invert=FALSE,
    NumBefore="^Ratio ",NumAfter="/",DenBefore="/",DenAfter="$",check.names=FALSE)
  position_ratio <- data.frame(Method="Position in String",Separators="\\s+|/",Invert=FALSE,
    NumPos=2L,DenPos=3L,check.names=FALSE)
  recognition_ratio <- data.frame(Method="Pattern Recognition",Separators="\\s+|/",Invert=FALSE,
    check.names=FALSE)
  stopifnot(identical(unname(unlist(ratio_extract("Ratio A/B",regex_ratio))),c("A","B")),
    identical(unname(unlist(ratio_extract("Ratio A/B",position_ratio))),c("A","B")),
    identical(unname(unlist(ratio_extract("Ratio A/B",recognition_ratio,c("A","B")))),c("A","B")))

  # One-row classes use the full candidate pipeline.  These fixtures require,
  # respectively, a protected literal, a generalized shape, an anchor, and an
  # exclusion; identical input assigned to two labels remains unresolvable.
  one_content <- infer_content(data.frame(Column=c("only_A","other_B"),Content=c("One","Other")),0L)
  two_content <- infer_content(data.frame(Column=c("two_A","two_B","other_A","other_B"),
    Content=c("Two","Two","Other","Other")),0L)
  stopifnot(all(one_content$status$Status=="reliable"),
    all(one_content$status$PositiveExamples==1L),
    !any(grepl("insufficient",one_content$warnings,ignore.case=TRUE)),
    all(two_content$status$Status=="reliable"))
  literal_one<-infer_content(data.frame(Column=c("Alpha","Beta"),Content=c("Literal","Background")),0L)
  structural_one<-infer_content(data.frame(Column=c("sample_123","other_ABC"),Content=c("Structural","Background")),0L)
  anchor_one<-infer_content(data.frame(Column=c("cat","catfish"),Content=c("Anchored","Background")),0L)
  exclusion_one<-infer_content(data.frame(Column=c("sample_A","sample_B"),Content=c("Excluded","Background")),0L)
  literal_rule<-literal_one$table[literal_one$table$Content=="Literal",,drop=FALSE]
  structural_rule<-structural_one$table[structural_one$table$Content=="Structural",,drop=FALSE]
  anchor_rule<-anchor_one$table[anchor_one$table$Content=="Anchored",,drop=FALSE]
  exclusion_rule<-exclusion_one$table[exclusion_one$table$Content=="Excluded",,drop=FALSE]
  stopifnot(nrow(literal_rule)==1L,nrow(structural_rule)==1L,
    grepl("[[:",structural_rule$Include,fixed=TRUE),nrow(anchor_rule)==1L,
    grepl("^",anchor_rule$Include,fixed=TRUE)||grepl("$",anchor_rule$Include,fixed=TRUE),
    nrow(exclusion_rule)==1L,nzchar(exclusion_rule$Exclude))
  conflict_content <- infer_content(data.frame(Column=c("same","same"),Content=c("A","B")),0L)
  stopifnot(all(conflict_content$status$Status=="unresolved"),
    all(conflict_content$unresolved_reasons$Code=="conflicting_labels"),
    sum(conflict_content$metrics$FN)==2L)
  summary_fixture<-data.frame(Column=c("alpha","beta","solo","overlap"),
    Content=c("A","B","Solo","B"),stringsAsFactors=FALSE)
  summary_rules<-data.frame(Content=c("A","B","Wrong"),Include=c("alpha","beta|overlap","overlap"),
    Exclude="",Transformation="None",stringsAsFactors=FALSE,check.names=FALSE)
  summary_application<-apply_content_table(summary_fixture,summary_rules)
  final_counts<-content_assignment_summary(summary_application$rows,summary_rules)
  stopifnot(final_counts$assigned_correctly==2L,final_counts$false_positives==1L,
    final_counts$false_negatives==2L,final_counts$unresolved_rows==1L,
    final_counts$conflicts==1L,final_counts$labels_with_no_selected_rule==1L,
    identical(final_counts$labels_without_selected_rule,"Solo"),
    final_counts$assigned_correctly+final_counts$false_positives+final_counts$unresolved_rows==nrow(summary_application$rows),
    final_counts$false_positives==sum(nzchar(summary_application$rows$Predicted)&!summary_application$rows$Match),
    final_counts$false_negatives==sum(nzchar(summary_application$rows$Expected)&!summary_application$rows$Match))
  stopifnot(identical(vapply(c(".","/","_","["),.format_special_character,character(1)),
    c("period (.)","slash (/)","underscore (_)","left bracket ([)")))
  row_index_fit<-infer_content(data.frame(Column=c("Row Index","value"),
    Content=c("Row Index","Value")),0L)
  canonical<-row_index_fit$table[row_index_fit$table$Content=="Row Index",,drop=FALSE]
  stopifnot(nrow(canonical)==1L,canonical$Include=="Row Index",canonical$Exclude=="",
    is.na(canonical$Transformation),
    row_index_fit$status$Status[row_index_fit$status$Content=="Row Index"]=="reliable")
  tie <- data.frame(Tier=rep("reliable",2),FN=0L,FP=0L,CrossValidationRecall=1,
    Specificity=1,ConstraintCount=1L,Complexity=0L,RegexLength=1L,
    Generalization=1,Include=c("b","a"),Exclude="",CandidateID=2:1)
  stopifnot(identical(.refinement_order(tie),c(2L,1L)),
    identical(candidate_fragments(golden[1:2],golden[3:4]),general))

  invalid <- x; invalid$table$Include[1L] <- "("
  excessive <- x; excessive$table$Include[1L] <- paste(rep("a",MAX_REGEX_LENGTH+1L),collapse="")
  stopifnot(any(grepl("invalid for PCRE",validate_export(invalid),fixed=TRUE)),
    any(grepl("may not exceed",validate_export(excessive),fixed=TRUE)))

  # Emission thresholds, event-key deduplication, external-recorder recursion
  # protection, truncation notification, and oldest-first eviction.
  threshold_logger <- create_session_logger(threshold="warning")
  stopifnot(threshold_logger("info",message="retained-not-emitted"),
    threshold_logger("warning",message="kept",event_key="once"),
    !threshold_logger("warning",message="duplicate",event_key="once"),
    length(attr(threshold_logger,"records")())==2L)
  recursive_logger <- NULL
  assign(".miraprot_log_record",function(...) recursive_logger("error",message="nested"),envir=.GlobalEnv)
  recursive_logger <- create_session_logger(threshold="trace")
  recursive_logger("error",message="outer")
  stopifnot(identical(vapply(attr(recursive_logger,"records")(),`[[`,character(1),"message"),c("outer","nested")))
  long_logger <- create_session_logger(threshold="trace")
  long_logger("info",message=paste(rep("x",MIRAPROT_MAX_LOG_MESSAGE_SIZE+1L),collapse=""))
  stopifnot(length(attr(long_logger,"records")())==2L,
    nchar(attr(long_logger,"records")()[[1L]]$message)==MIRAPROT_MAX_LOG_MESSAGE_SIZE)

  # Every shipped template is a golden compatibility fixture for the effective
  # Data Wizard loading contract, not merely for saveRDS/readRDS serialization.
  script_arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  script_dir <- if(length(script_arg))dirname(normalizePath(sub("^--file=","",script_arg[[1L]]))) else getwd()
  fixture_dir <- file.path(script_dir,"AutoAssign")
  shipped <- sort(list.files(fixture_dir,pattern="\\.rds$",full.names=TRUE))
  stopifnot(length(shipped)>0L)
  expected_classes <- list(table=rep("character",4L),condition=c(rep("character",5L),"integer"),
    ratio=c("character","character","character","logical",rep("character",4L),"integer","integer"))
  for(path in shipped) {
    original <- readRDS(path); stopifnot(is.list(original),
      identical(names(original),c("table","condition","ratio","debug_info")))
    core <- original[c("table","condition","ratio")]
    stopifnot(identical(names(core$table),CONTENT_FIELDS),identical(names(core$condition),CONDITION_FIELDS),
      identical(names(core$ratio),RATIO_FIELDS))
    for(component in names(core)) stopifnot(identical(unname(vapply(core[[component]],function(z)class(z)[1L],character(1))),expected_classes[[component]]))
    normalized_fixture <- data_wizard_normalize_rules(original)
    stopifnot(sum(core$table$Content=="Row Index",na.rm=TRUE)==1L,
      identical(normalized_fixture$condition$Separators,core$condition$Separators),
      identical(normalized_fixture$ratio$Separators,core$ratio$Separators))
    rr <- core$ratio
    stopifnot(all(is.na(rr$Separators[rr$Method=="Regular Expressions"])),
      all(is.na(rr$NumPos[rr$Method!="Position in String"])),
      all(is.na(rr$DenPos[rr$Method!="Position in String"])),
      all(is.na(rr$NumBefore[rr$Method!="Regular Expressions"])),
      all(is.na(rr$NumAfter[rr$Method!="Regular Expressions"])),
      all(is.na(rr$DenBefore[rr$Method!="Regular Expressions"])),
      all(is.na(rr$DenAfter[rr$Method!="Regular Expressions"])))
    temp <- tempfile(fileext=".rds"); saveRDS(original,temp); reloaded_fixture<-readRDS(temp);unlink(temp)
    stopifnot(identical(original,reloaded_fixture),
      identical(data_wizard_normalize_rules(original),data_wizard_normalize_rules(reloaded_fixture)))
  }

  # Performance safety is primarily a deterministic work bound.  Wall-clock
  # checks are deliberately generous to avoid treating slow CI hosts as errors.
  large_metadata <- data.frame(Column=sprintf("Intensity Sample%05d (R%d)",seq_len(2000L),(seq_len(2000L)-1L)%%3L+1L),
    Content=rep(c("Odd","Even"),1000L),stringsAsFactors=FALSE)
  started <- proc.time()[["elapsed"]]
  bounded <- candidate_fragments(large_metadata$Column[1:1000],large_metadata$Column[1001:2000],
    CANDIDATE_FRAGMENT_SEARCH_LIMIT)
  elapsed <- proc.time()[["elapsed"]]-started
  stopifnot(length(bounded)<=CANDIDATE_FRAGMENT_SEARCH_LIMIT,elapsed<120)
  perf_refined <- refine_pattern_search("Intensity",large_metadata$Column,
    large_metadata$Content=="Odd",max_candidates=REFINEMENT_CANDIDATE_LIMIT)
  stopifnot(nrow(perf_refined$candidates)<=REFINEMENT_CANDIDATE_LIMIT)

  # Application lifecycle harness.  Stop/run functions are injected so these
  # checks cannot affect a host Shiny application or open a listening socket.
  reset_lifecycle <- function(standalone=FALSE, running=FALSE) {
    cleanup_application_references()
    application_lifecycle$app_started <- running
    application_lifecycle$ever_had_session <- FALSE
    application_lifecycle$active_sessions <- new.env(hash=TRUE,parent=emptyenv())
    application_lifecycle$shutdown_requested <- FALSE
    application_lifecycle$shutdown_completed <- FALSE
    application_lifecycle$shutdown_in_progress <- FALSE
    application_lifecycle$temporary_paths <- character()
    application_lifecycle$cached_artifacts <- list()
    application_lifecycle$session_log_references <- list()
    application_lifecycle$scheduled_callbacks <- list()
    application_lifecycle$standalone_process <- standalone
    application_lifecycle$auto_stop_last_session <- TRUE
    application_lifecycle$application_running <- running
  }
  stop_calls <- 0L
  application_lifecycle$stop_application <- function() stop_calls <<- stop_calls+1L
  register_test_session <- function(id) {
    record<-new.env(parent=emptyenv());record$ended<-FALSE
    record$temporary_files<-character();record$clear_log<-function()NULL
    assign(as.character(id),record,envir=application_lifecycle$active_sessions)
    application_lifecycle$ever_had_session <- TRUE
  }
  active_count <- function() length(ls(application_lifecycle$active_sessions,all.names=TRUE))

  # Merely starting with no browser connection is not a shutdown event.
  reset_lifecycle(TRUE,TRUE)
  stopifnot(active_count()==0L,!application_lifecycle$ever_had_session,
    !application_lifecycle$shutdown_requested,stop_calls==0L)

  register_test_session("one")
  stopifnot(application_lifecycle$ever_had_session)
  stopifnot(active_count()==1L,end_application_session("one"),active_count()==0L,
    !end_application_session("one"),stop_calls==1L,
    application_lifecycle$shutdown_requested,application_lifecycle$shutdown_completed,
    !shutdown_application("duplicate"),stop_calls==1L)

  reset_lifecycle(TRUE,TRUE); register_test_session("first");register_test_session("second")
  previous_stops<-stop_calls
  stopifnot(end_application_session("first"),active_count()==1L,stop_calls==previous_stops)
  end_application_session("second")
  stopifnot(stop_calls==previous_stops+1L)

  reset_lifecycle(FALSE,TRUE);register_test_session("embedded")
  previous_stops<-stop_calls;end_application_session("embedded")
  stopifnot(stop_calls==previous_stops,!application_lifecycle$shutdown_requested)

  reset_lifecycle(TRUE,TRUE);application_lifecycle$auto_stop_last_session<-FALSE
  register_test_session("reconnectable");previous_stops<-stop_calls
  end_application_session("reconnectable")
  stopifnot(stop_calls==previous_stops,!application_lifecycle$shutdown_requested)

  reset_lifecycle(TRUE,TRUE)
  temporary_path<-tempfile();writeLines("owned",temporary_path)
  application_lifecycle$temporary_paths<-temporary_path
  application_lifecycle$cached_artifacts<-list(artifact=raw(100L))
  application_lifecycle$session_log_references<-list(log=function()NULL)
  previous_stops<-stop_calls
  stopifnot(shutdown_application("onStop harness",stop_app=FALSE),
    stop_calls==previous_stops,!file.exists(temporary_path),
    !length(application_lifecycle$temporary_paths),
    !length(application_lifecycle$cached_artifacts),
    !length(application_lifecycle$session_log_references))

  # Model sourced interactive execution: runApp blocks while connected, the
  # last-session callback stops it, and the runner then returns to its caller.
  reset_lifecycle(TRUE,TRUE);runner_returned<-FALSE;previous_stops<-stop_calls
  fake_runner<-function(app,launch.browser){
    register_test_session("sourced")
    end_application_session("sourced")
    runner_returned<<-TRUE
    invisible(NULL)
  }
  stopifnot(is.null(run_application(list(),FALSE,endpoint=list(),runner=fake_runner)),runner_returned,
    stop_calls==previous_stops+1L,application_lifecycle$shutdown_completed)
  reset_lifecycle(FALSE,FALSE)
  startup_lifecycle_self_tests()
  TRUE
}

browser_launch_enabled <- function(args = commandArgs(trailingOnly = TRUE)) {
  setting <- tolower(trimws(Sys.getenv("REGEX_METADATA_ASSISTANT_LAUNCH_BROWSER", "true")))
  !"--no-browser" %in% args && !setting %in% c("0", "false", "no", "off")
}

browser_launcher <- function(enabled) {
  function(url) {
    message("Regex Metadata Assistant is ready at: ", url)
    if (!enabled) {
      message("Automatic browser launch is disabled. Open the URL above manually.")
      return(invisible(NULL))
    }
    result <- tryCatch(utils::browseURL(url), error=function(error) error)
    if (inherits(result, "error") || (length(result) && is.numeric(result) && result != 0)) {
      detail <- if (inherits(result, "error")) conditionMessage(result) else paste("browser command returned status", result)
      message("Automatic browser launch failed (", detail, "). Open the URL above manually: ", url)
    } else {
      message("If no browser window opens, open the URL above manually.")
    }
    invisible(NULL)
  }
}

keep_server_available <- function() {
  setting <- tolower(trimws(Sys.getenv("REGEX_METADATA_ASSISTANT_KEEP_ALIVE", "false")))
  setting %in% c("1", "true", "yes", "on")
}

application_endpoint <- function() {
  host <- trimws(Sys.getenv("REGEX_METADATA_ASSISTANT_HOST", ""))
  port_text <- trimws(Sys.getenv("REGEX_METADATA_ASSISTANT_PORT", ""))
  endpoint <- list()
  if (nzchar(host)) endpoint$host <- host
  if (nzchar(port_text)) {
    port <- suppressWarnings(as.integer(port_text))
    if (is.na(port) || port < 1L || port > 65535L)
      stop("REGEX_METADATA_ASSISTANT_PORT must be an integer from 1 through 65535.", call.=FALSE)
    endpoint$port <- port
  }
  endpoint
}

run_application <- function(app, launch_browser, endpoint=application_endpoint(), runner=shiny::runApp) {
  # Omitting port (rather than assigning a fixed default) lets Shiny select an
  # available port.  The callback runs only after that URL is known.
  do.call(runner, c(list(app), list(launch.browser=browser_launcher(launch_browser)), endpoint))
  invisible(NULL)
}

launch_regex_metadata_assistant <- function(args = if (invoked_through_rscript) commandArgs(trailingOnly=TRUE) else character()) {
  application_lifecycle$main_invocations <- application_lifecycle$main_invocations + 1L
  if (application_lifecycle$main_invocations != 1L) stop("Private launcher may only be invoked once.", call.=FALSE)
  application_lifecycle$standalone_process <- TRUE
  application_lifecycle$auto_stop_last_session <- !keep_server_available()
  application_lifecycle$application_running <- TRUE
  on.exit({
    application_lifecycle$application_running <- FALSE
    shutdown_application("main returned", stop_app=FALSE)
  }, add=TRUE)
  .miraprot_bootstrap_log("info", "startup", "bootstrap", "Preparing Regex Metadata Assistant startup.", "startup-begin")
  ensure_packages()
  launch_browser <- browser_launch_enabled(args)
  browser_message <- if (launch_browser) {
    "A browser window will open automatically."
  } else {
    "Browser launch is disabled; open the listening URL printed below manually."
  }
  .miraprot_bootstrap_log("info", "startup", "initialize",
    paste("Starting MiraProt Regex Metadata Assistant.", browser_message), "startup-run-app")
  app <- shiny::shinyApp(build_ui(), server, onStart=function() {
    application_lifecycle$app_started <- TRUE
    shiny::onStop(function() shutdown_application("Shiny application stopped", stop_app=FALSE))
  })
  announce_startup()
  tryCatch(run_application(app, launch_browser), error=function(error) {
    message("Regex Metadata Assistant failed to start: ", conditionMessage(error))
    stop(error)
  })
}

# Capture source-versus-command-line execution once.  This distinction is
# informational, prevents a sourced copy from consuming RStudio's unrelated
# process arguments, and controls whether process-level termination is permissible.
is_command_line_invocation <- function(args=commandArgs(FALSE)) {
  file_arguments <- grep("^--file=", args, value=TRUE)
  if (length(file_arguments) != 1L) return(FALSE)
  path <- sub("^--file=", "", file_arguments[[1L]])
  path <- gsub('^(["\'])|(["\'])$', "", path)
  path <- gsub("\\\\", "/", path)
  identical(tolower(tail(strsplit(path, "/", fixed=TRUE)[[1L]], 1L)),
    tolower("Regex_Metadata_Assistant.R"))
}
invoked_through_rscript <- is_command_line_invocation()
self_test_requested <- function() identical(Sys.getenv("REGEX_METADATA_ASSISTANT_SELF_TEST"), "1")

dispatch_startup <- function(self_test, launcher=launch_regex_metadata_assistant,
                             tester=run_self_tests) {
  if (self_test) {
    tester()
    message("Regex Metadata Assistant self-tests passed.")
  } else {
    launcher()
  }
  invisible(NULL)
}

announce_startup <- function() {
  policy <- if (isTRUE(application_lifecycle$auto_stop_last_session)) {
    "The app will stop after the last connected session ends and return control to the R prompt."
  } else {
    "The server will remain available for reconnects after the last session ends."
  }
  message("Starting Regex Metadata Assistant... ", policy,
    " Set REGEX_METADATA_ASSISTANT_KEEP_ALIVE=true to keep it available for reconnects.")
}

startup_lifecycle_self_tests <- function() {
  # Source and Rscript use the same dispatcher; detection is deliberately not
  # an application-launch gate.
  launch_calls <- 0L
  test_launcher <- function() launch_calls <<- launch_calls + 1L
  dispatch_startup(FALSE, test_launcher, function() stop("unexpected test"))
  dispatch_startup(FALSE, test_launcher, function() stop("unexpected test"))
  stopifnot(launch_calls == 2L)

  test_calls <- 0L
  dispatch_startup(TRUE, function() stop("launcher reached"),
    function() test_calls <<- test_calls + 1L)
  stopifnot(test_calls == 1L,
    is_command_line_invocation("--file=C:\\R local\\MiraProt\\Regex_Metadata_Assistant.R"),
    is_command_line_invocation('--file="C:\\R local\\MiraProt\\Regex_Metadata_Assistant.R"'),
    !is_command_line_invocation("--file=C:\\R local\\MiraProt\\different.R"))

  # A failed sourced launch is an ordinary R condition, and a returning runner
  # hands control back to its caller.  Neither route performs process exit.
  source_failure <- tryCatch({
    dispatch_startup(FALSE, function() stop("source failure", call.=FALSE))
    NULL
  }, error=identity)
  returned_to_caller <- FALSE
  dispatch_startup(FALSE, function() returned_to_caller <<- TRUE)
  stopifnot(inherits(source_failure, "error"),
    identical(conditionMessage(source_failure), "source failure"), returned_to_caller)

  startup_output <- capture.output(announce_startup(), type="message")
  runner_calls <- 0L
  browser_argument <- NULL
  runner_arguments <- NULL
  fake_runner <- function(app, launch.browser, ...) {
    runner_calls <<- runner_calls + 1L
    browser_argument <<- launch.browser
    runner_arguments <<- list(...)
  }
  run_application(list(), FALSE, endpoint=list(), runner=fake_runner)
  disabled_output <- capture.output(browser_argument("http://127.0.0.1:1234"), type="message")
  stopifnot(is.function(browser_argument), !length(runner_arguments),
    any(grepl("ready at: http://127.0.0.1:1234", disabled_output, fixed=TRUE)),
    any(grepl("Open the URL above manually", disabled_output, fixed=TRUE)))
  run_application(list(), TRUE, endpoint=list(host="0.0.0.0",port=4321L), runner=fake_runner)
  stopifnot(length(startup_output)==1L,
    grepl("return control to the R prompt",startup_output,fixed=TRUE),
    runner_calls == 2L, is.function(browser_argument),
    identical(runner_arguments,list(host="0.0.0.0",port=4321L)))
  invisible(TRUE)
}

# This assertion also runs in ordinary source/non-launch mode, proving that the
# wrapper has not leaked representative implementation bindings globally.
private_bindings <- c("required_packages","package_repository","MIRAPROT_LOG_LEVELS",
  "MIRAPROT_LOG_COMPONENTS","ensure_packages","regex_escape_literal","infer_content",
  "build_ui","server","application_lifecycle","cleanup_application_references",
  "run_self_tests","launch_regex_metadata_assistant")
stopifnot(!any(vapply(private_bindings, exists, logical(1), envir=globalenv(), inherits=FALSE)))

# This must be the final expression: private closures are still alive when a
# sourced copy enters runApp(), and normal return hands control back to its
# caller without ever terminating the R process.
dispatch_startup(self_test_requested())
})
