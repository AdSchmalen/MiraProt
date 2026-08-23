# ============================================================================
# Module Script: R/session_save_restore.R
# Purpose:
#   Parent loader for the refactored session save/restore subsystem.
#
# Architectural Role:
#   composition root
#
# Responsibilities:
#   - Source session save/restore sub-scripts into the active module environment.
#   - Keep the public API surface unchanged for callers (app.R, tests, modules).
#
# Non-Responsibilities (Must NOT be here):
#   - Implement save/restore workflows directly.
#   - Define module-specific state contracts.
# ============================================================================

.sr_source_dir <- local({
  src_file <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
  if (!is.character(src_file) || length(src_file) != 1L || !nzchar(src_file)) {
    return(file.path("R", "session_save_restore"))
  }
  file.path(dirname(src_file), "session_save_restore")
})

.sr_subscripts <- c(
  "session_save_restore_core_helpers.R",
  "session_save_restore_callbacks.R",
  "session_save_restore_module_registration.R",
  "session_save_restore_orchestration.R"
)

.sr_env <- if (exists("modEnv", envir = globalenv(), inherits = FALSE)) {
  get("modEnv", envir = globalenv(), inherits = FALSE)
} else {
  environment()
}

for (.sr_subscript in .sr_subscripts) {
  .sr_subscript_path <- file.path(.sr_source_dir, .sr_subscript)
  if (!file.exists(.sr_subscript_path)) {
    stop("Missing session save/restore sub-script: ", .sr_subscript_path)
  }
  sys.source(.sr_subscript_path, envir = .sr_env)
}

if (!exists(".session_save_restore_api", envir = .sr_env, inherits = FALSE)) {
  stop("Session save/restore API manifest (.session_save_restore_api) is missing.")
}

.sr_api_manifest <- get(".session_save_restore_api", envir = .sr_env, inherits = FALSE)
.sr_public_symbols <- .sr_api_manifest$public_api
.sr_legacy_aliases <- .sr_api_manifest$legacy_aliases
.sr_internal_symbols <- .sr_api_manifest$internal_helpers

.sr_required_function_symbols <- c(
  "setup_session_save_restore",
  "create_session_registry",
  "register_module_session_participants"
)

.sr_required_constant_specs <- list(
  MIRAPROT_SESSION_SCHEMA_VERSION = function(x) is.character(x) && length(x) == 1L,
  MIRAPROT_SESSION_COMPATIBLE_VERSIONS = function(x) is.character(x) && length(x) >= 1L,
  MIRAPROT_APP_VERSION = function(x) is.character(x) && length(x) == 1L,
  SESSION_SAVE_LEVEL_DATA = function(x) is.character(x) && length(x) == 1L,
  SESSION_SAVE_LEVEL_ANALYSIS = function(x) is.character(x) && length(x) == 1L,
  SESSION_SAVE_LEVEL_FULL = function(x) is.character(x) && length(x) == 1L
)

.sr_fail <- function(kind, symbol, detail) {
  stop(sprintf("Session save/restore integrity check failed [%s]: %s (%s)", kind, symbol, detail), call. = FALSE)
}

# Integrity table: required functions
for (.sr_fn_symbol in .sr_required_function_symbols) {
  if (!exists(.sr_fn_symbol, envir = .sr_env, inherits = FALSE)) {
    .sr_fail("missing-function", .sr_fn_symbol, "symbol not found")
  }
  .sr_fn_value <- get(.sr_fn_symbol, envir = .sr_env, inherits = FALSE)
  if (!is.function(.sr_fn_value)) {
    .sr_fail("invalid-function", .sr_fn_symbol, paste0("expected function, got ", typeof(.sr_fn_value)))
  }
}

# Integrity table: required constants
for (.sr_const_symbol in names(.sr_required_constant_specs)) {
  if (!exists(.sr_const_symbol, envir = .sr_env, inherits = FALSE)) {
    .sr_fail("missing-constant", .sr_const_symbol, "symbol not found")
  }
  .sr_const_value <- get(.sr_const_symbol, envir = .sr_env, inherits = FALSE)
  .sr_const_validator <- .sr_required_constant_specs[[.sr_const_symbol]]
  if (!isTRUE(.sr_const_validator(.sr_const_value))) {
    .sr_fail("invalid-constant", .sr_const_symbol, paste0("unexpected type/shape; got ", typeof(.sr_const_value)))
  }
}

for (.sr_public_symbol in .sr_public_symbols) {
  if (!exists(.sr_public_symbol, envir = .sr_env, inherits = FALSE)) {
    stop("Missing required public session symbol: ", .sr_public_symbol)
  }
  assign(.sr_public_symbol, get(.sr_public_symbol, envir = .sr_env), envir = environment())
}

for (.sr_legacy_symbol in .sr_legacy_aliases) {
  if (exists(.sr_legacy_symbol, envir = .sr_env, inherits = FALSE)) {
    assign(.sr_legacy_symbol, get(.sr_legacy_symbol, envir = .sr_env), envir = environment())
  }
}

.sr_allowed_global_symbols <- c("DEBUG_LEVEL", "MIRAPROT_SESSION_TOKEN")
.sr_global_symbols <- ls(envir = globalenv(), all.names = TRUE)
.sr_remove_globals <- setdiff(
  intersect(.sr_global_symbols, c(.sr_internal_symbols, names(.sr_required_constant_specs))),
  .sr_allowed_global_symbols
)
if (length(.sr_remove_globals) > 0L && !identical(.sr_env, globalenv())) {
  rm(list = .sr_remove_globals, envir = globalenv())
}

rm(list = intersect(c(
  ".sr_source_dir",
  ".sr_subscripts",
  ".sr_env",
  ".sr_api_manifest",
  ".sr_public_symbols",
  ".sr_legacy_aliases",
  ".sr_internal_symbols",
  ".sr_required_function_symbols",
  ".sr_required_constant_specs",
  ".sr_allowed_global_symbols",
  ".sr_global_symbols",
  ".sr_remove_globals",
  ".sr_fn_symbol",
  ".sr_fn_value",
  ".sr_const_symbol",
  ".sr_const_value",
  ".sr_const_validator",
  ".sr_public_symbol",
  ".sr_legacy_symbol",
  ".sr_subscript",
  ".sr_subscript_path",
  ".sr_fail"
), ls(envir = environment(), all.names = TRUE)))
