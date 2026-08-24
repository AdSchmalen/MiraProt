# app.R
# Libraries, options, DEBUG_LEVEL, and debug_log() are defined in R/bootstrap.R
# (auto-sourced by Shiny before this file runs)

# ========================================
# Session Management Initialization
# ========================================
# R/session_management.R is auto-sourced by Shiny (no explicit source() needed)

old_warn <- getOption("warn")
options(warn = -1)  # Suppress warnings during initialization
options(DT.warn.size = FALSE)
run_orphan_cleanup <- should_run_orphan_cleanup_from_lock(stale_after_seconds = 300)
if (run_orphan_cleanup) {
  debug_log("Stale session lock detected; enabling orphan cleanup", 2)
} else {
  debug_log("No stale session lock detected; skipping orphan cleanup", 2)
}
initialize_session_management(clean_orphaned = run_orphan_cleanup)
write_session_lock_file()
cat("[ SESSION ] Cleanup completed successfully\n")
options(warn = old_warn)


# ========================================
# ROBUST Module Environment Setup
# ========================================


# Safe modEnv handling
if (exists("modEnv", envir = globalenv())) {
  # Clear existing content instead of removing environment
  tryCatch({
    existing_objects <- ls(envir = modEnv, all.names = TRUE)
    if (length(existing_objects) > 0) {
      rm(list = existing_objects, envir = modEnv)
    }
  }, error = function(e) {
    debug_log(paste("Could not clear modEnv, creating new:", e$message), 1)
    modEnv <- new.env(parent = globalenv())
    assign("modEnv", modEnv, envir = globalenv())
  })
} else {
  modEnv <- new.env(parent = globalenv())
  assign("modEnv", modEnv, envir = globalenv())
}


# Load all module files (SAME AS BEFORE)
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) {
  tryCatch({
    sys.source(f, envir = modEnv)
    debug_log(paste("Loaded:", basename(f)), 2)
  }, error = function(e) {
    message <- sprintf("Required module source failed (%s): %s",
                       normalizePath(f, winslash = "/", mustWork = FALSE),
                       conditionMessage(e))
    # A failed source can leave only a prefix of the module's objects in
    # modEnv. Continuing then turns the real load error into an opaque
    # `dots_list(): attempt to apply non-function` while build_ui() forces a
    # missing UI constructor. Abort here and preserve the originating error.
    stop(message, call. = FALSE)
  })
}

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)) {
  if (!basename(f) %in% c("utils.R", "bootstrap.R", "ui.R", "server_modules.R", "server_coordination.R", "server_export.R", "server_diagnostics.R", "session_save_restore.R")) {
    tryCatch({
      sys.source(f, envir = modEnv)
      debug_log(paste("Loaded:", basename(f)), 2)
    }, error = function(e) {
      debug_log(paste("Error loading", basename(f), ":", e$message), 1)
    })
  }
}

# Load documentation through its dependency-aware loader. MiraProt content
# renderers are sourced and validated before its UI/server router is registered.
get("load_documentation_files", envir = modEnv, inherits = TRUE)(
  envir = modEnv,
  logger = debug_log
)


# Load GSEA export functions at the top of app.R with other source statements
if (file.exists("./modules/GSEA/GSEA_export.R")) {
  sys.source("./modules/GSEA/GSEA_export.R", envir = modEnv)
} else {
  debug_log("GSEA_export.R not found, GSEA Excel export will be unavailable", 1)
}

# ========================================
# UI Definition (see R/ui.R)
# ========================================

ui <- build_ui(modEnv)

assert_required_callable <- function(symbol_name, symbol_env = environment()) {
  if (!exists(symbol_name, envir = symbol_env, inherits = TRUE)) {
    stop(sprintf("Missing required session save/restore symbol: %s", symbol_name), call. = FALSE)
  }
  symbol_value <- get(symbol_name, envir = symbol_env, inherits = TRUE)
  if (!is.function(symbol_value)) {
    stop(sprintf("Required session save/restore symbol is not a function: %s", symbol_name), call. = FALSE)
  }
  invisible(TRUE)
}

required_session_symbols <- c(
  "setup_session_save_restore",
  "create_session_registry",
  "register_module_session_participants"
)
for (required_symbol in required_session_symbols) {
  assert_required_callable(required_symbol)
}
stopifnot(
  is.function(setup_session_save_restore),
  is.function(create_session_registry),
  is.function(register_module_session_participants)
)

server <- function(input, output, session) {

  # New session lifecycle: allow one cleanup execution for this shutdown path.
  cleanup_manager$reset_shutdown_guard()

  # ========================================
  # Health Endpoint (portable mode)
  # ========================================

  if (MIRAPROT_IN_PORTABLE) {
    health_on_session_start()
  }

  # ========================================
  # Reactive Values Setup
  # ========================================

  rv <- reactiveValues()

  # ========================================
  # Session Registry Setup
  # ========================================
  # Create the session registry before module initialization so that
  # modules can register their save/restore participants during init.

  session_registry <- create_session_registry()

  # ========================================
  # Module Initialization (see R/server_modules.R)
  # ========================================

  module_outputs <- reactiveValues()
  initialize_all_modules(module_outputs, rv, modEnv)

  # ========================================
  # Module Session Registration (see R/session_save_restore.R)
  # ========================================
  # Register each module as a session participant after initialization.
  # Priority controls restore order (lower = earlier):
  #   10 = Data Wizard (must restore first so data flows to downstream)
  #   30 = Analysis modules (GO, GSEA)
  #   50 = Visualization modules

  register_module_session_participants(session_registry, module_outputs,
                                       rv = rv, session = session)

  # ========================================
  # Reactive Coordination (see R/server_coordination.R)
  # ========================================
  # Sets up: window size tracking, GO sync, module status tracking

  coordination <- setup_reactive_coordination(input, output, rv, module_outputs, modEnv)
  module_status <- coordination$module_status

  # ========================================
  # Excel Export (see R/server_export.R)
  # ========================================
  # Sets up: download_results handler, export_status output

  setup_export_handlers(output, rv, module_outputs)

  # ========================================
  # Diagnostics (see R/server_diagnostics.R)
  # ========================================
  # Sets up: debug_module_status, module_status_detailed, debug_info outputs,
  #          startup health checks (prerequisites + file existence)

  setup_diagnostics(input, output, session, module_outputs, module_status, modEnv)

  # ========================================
  # Session Save/Restore (see R/session_save_restore.R)
  # ========================================
  # Sets up: session_download handler, session_file upload validation,
  #          auto-restore on validation, session_restore_status output

  setup_session_save_restore(input, output, session, rv,
                            session_registry = session_registry)

  # ========================================
  # Session Cleanup (Enhanced)
  # ========================================

  session$onSessionEnded(function() {
    t0 <- proc.time()[3]
    debug_log("App session ending - executing centralized cleanup", 2)

    # Update health endpoint session counter
    if (MIRAPROT_IN_PORTABLE) {
      health_on_session_end()
    }

    # This now includes ALL cleanup: modules + system + your current operations
    cleanup_manager$execute_cleanup(mode = "session_end")
    if (exists("modEnv", envir = globalenv(), inherits = FALSE)) {
      assign(".cleanup_executed", TRUE, envir = modEnv)
    }

    # Remove session lock file during normal shutdown
    remove_session_lock_file()

    elapsed_ms <- round((proc.time()[3] - t0) * 1000)
    debug_log(sprintf("App session cleanup completed in %dms", elapsed_ms), 2)

    # In local desktop mode we stop the app on browser close; clear modEnv
    # beforehand so large per-session objects do not survive until next launch.
    if (isTRUE(getOption("miraprot.stop_on_close", TRUE)) &&
        exists("modEnv", envir = globalenv(), inherits = FALSE)) {
      tryCatch({
        rm(list = ls(envir = modEnv, all.names = TRUE), envir = modEnv)
        rm("modEnv", envir = globalenv())
      }, error = function(e) {
        debug_log(paste("modEnv cleanup warning:", e$message), 1)
      })
    }

    # Optional: stop app on browser close (defaults to TRUE for local desktop usage).
    # Deferred via later::later() so that this onSessionEnded callback returns
    # before the stop signal is sent to the Shiny event loop. Calling stopApp()
    # synchronously inside onSessionEnded blocks the event loop and causes RStudio
    # to become unresponsive.
    if (isTRUE(getOption("miraprot.stop_on_close", TRUE))) {
      later::later(shiny::stopApp, delay = 0)
    }
  })
}

# ========================================
# Create and Return Shiny App
# ========================================

# Register lightweight process-wide cleanup on app stop
shiny::onStop(function() {
  remove_session_lock_file()

  # Final process-level pass (kept lightweight by default; see options
  # miraprot.cleanup.close_connections / miraprot.cleanup.run_gc).
  # cleanup_manager internally deduplicates repeated shutdown invocations.
  try(cleanup_manager$execute_cleanup(mode = "app_stop"), silent = TRUE)

  if (exists("modEnv", envir = globalenv(), inherits = FALSE)) {
    try(rm("modEnv", envir = globalenv()), silent = TRUE)
  }
})

app <- shinyApp(ui, server)

app
