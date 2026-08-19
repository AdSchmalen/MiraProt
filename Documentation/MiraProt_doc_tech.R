# ==============================================================================
# File: Documentation/MiraProt_doc_tech.R
#
# Purpose:
#   Technical documentation for the top-level MiraProt module.
#   Audience: developers and maintainers.
#   Maintainer note: this file is the owner for developer-facing documentation.
#   User-facing guidance belongs in Documentation/MiraProt_doc_user.R.
#   Routing and navigation belong in Documentation/MiraProt_doc_ui.R only.
# ==============================================================================

sp_doc_code_panel <- function(code_text) {
  tags$div(
    class = "sp-code-panel",
    tags$pre(code_text)
  )
}

#' Technical Documentation — Architecture
#' @keywords internal
render_tech_miraprot_architecture_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("MiraProt Architecture and Responsibilities"),
    div(
      class = "alert alert-secondary",
      tags$b("Documentation maintainer contract"),
      tags$ul(
        tags$li("Section ownership: user-facing content in ", tags$code("Documentation/MiraProt_doc_user.R"), ", developer-facing content in ", tags$code("Documentation/MiraProt_doc_tech.R"), ", routing-only wiring in ", tags$code("Documentation/MiraProt_doc_ui.R"), "."),
        tags$li("Synchronization rule: each navigation item must map to one concrete renderer function that returns non-empty, audience-specific content."),
        tags$li("Edit checklist: keep terminology consistent, avoid version-bound wording unless required, and remove placeholders/TODO text before merge."),
        tags$li("Policy: documentation avoids hardcoded 'current version' statements and points readers to script/README defaults for supported releases.")
      )
    ),
    tags$p("The top-level app is a thin orchestrator. app.R assembles UI and server infrastructure while modules encapsulate domain logic."),
    tags$h3("Core structure"),
    tags$ul(
      tags$li(tags$code("app.R"), ": startup orchestration, modEnv population, server delegation, session cleanup."),
      tags$li(tags$code("R/bootstrap.R"), ": package loading, options, debug utilities, portable-mode environment detection."),
      tags$li(tags$code("R/ui.R"), ": navbar layout, module tab composition, documentation tab embedding."),
      tags$li(tags$code("R/server_modules.R"), ": module initialization contracts and fault-tolerant startup."),
      tags$li(tags$code("R/server_coordination.R"), ": cross-module observers and shared reactive synchronization."),
      tags$li(tags$code("R/server_export.R"), ": unified Excel export handlers."),
      tags$li(tags$code("R/server_diagnostics.R"), ": diagnostics output and startup checks."),
      tags$li(tags$code("R/session_management.R"), ": lock-file based startup hygiene and process cleanup."),
      tags$li(tags$code("R/centralized_cleanup.R"), ": centralized shutdown cleanup manager.")
    ),
    tags$h3("Main UI areas and navigation logic"),
    tags$ol(
      tags$li("Main Analysis tab hosts all scientific modules in a nested tabsetPanel."),
      tags$li("System Info tab exposes window metrics and module status diagnostics."),
      tags$li("Documentation tab hosts per-module documentation modules, including MiraProt documentation."),
      tags$li("About tab contains project metadata.")
    ),
    tags$h3("Integration points between modules"),
    tags$ul(
      tags$li("Shared reactiveValues object rv acts as cross-module data bus."),
      tags$li("module_outputs stores module return objects and reactive accessors."),
      tags$li("GO and GSEA result reactives are forwarded into downstream modules such as Volcano, Dot Plot, Venn, Heatmap, and STRING."),
      tags$li("Export code accesses both rv and module_outputs to collect table snapshots.")
    ),
    tags$h3("Project layout"),
    tags$ul(
      tags$li(
        tags$code("R/"),
        ": app-level infrastructure and orchestration. Key files include ",
        tags$code("ui.R"),
        ", ",
        tags$code("server_modules.R"),
        ", ",
        tags$code("server_coordination.R"),
        ", ",
        tags$code("server_export.R"),
        ", and ",
        tags$code("bootstrap.R"),
        ". Extension points: add global coordination observers, shared startup utilities, and cross-cutting infrastructure that should not belong to one module."
      ),
      tags$li(
        tags$code("modules/"),
        ": module wrappers and per-module implementation trees. Key files include wrapper files such as ",
        tags$code("modules/*_module.R"),
        " and implementation subtrees under ",
        tags$code("modules/<ModuleName>/"),
        ". Extension points: add new module UI/server entrypoints and keep module internals isolated in their own subtree."
      ),
      tags$li(
        tags$code("Documentation/"),
        ": in-app developer and user documentation renderers. Key files include documentation modules and technical documentation pages such as this file. Extension points: add or update doc sections when architecture, data contracts, or workflows change."
      ),
      tags$li(
        tags$code("portable/"),
        ": packaging/runtime assets for desktop-style distribution. Key files include ",
        tags$code("portable/launcher/"),
        " and ",
        tags$code("portable/scripts/"),
        " build helpers. Extension points: update launcher behavior, installer logic, and prebuild/cache scripts for portable releases."
      ),
      tags$li(
        tags$code("GSEA/"),
        ": Gene Set Enrichment Analysis reference assets and module-facing resources. Key files typically include bundled gene set or enrichment support data. Extension points: add curated datasets/resources used by the GSEA workflow while keeping interfaces stable."
      ),
      tags$li(
        tags$code("Gene Ontology/"),
        ": Gene Ontology reference data and helpers consumed by GO analysis flows. Key files typically include ontology-related assets used by GO modules. Extension points: add/update ontology support assets with backward-compatible access patterns."
      ),
      tags$li(
        tags$code("AutoAssign/"),
        ": automatic assignment/classification support resources and logic inputs. Key files include AutoAssign data/config artifacts and helper scripts. Extension points: expand AutoAssign mappings/rules without coupling to unrelated modules."
      )
    ),
    tags$h3("Where to add a new module"),
    tags$ol(
      tags$li(
        "Create a wrapper in ",
        tags$code("modules/*_module.R"),
        " that exposes module UI and server entry functions."
      ),
      tags$li(
        "Create the implementation subtree in ",
        tags$code("modules/<ModuleName>/"),
        " for internal logic, helpers, and assets."
      ),
      tags$li(
        "Register module integration touchpoints in ",
        tags$code("R/ui.R"),
        " (navigation/layout) and ",
        tags$code("R/server_modules.R"),
        " (initialization, contract wiring, and output registration)."
      )
    ),
    tags$h3("Guardrails for maintainable integration"),
    tags$ul(
      tags$li("Avoid hidden cross-module dependencies; modules should not reach into each other's private implementation state."),
      tags$li("Use explicit contracts through ", tags$code("rv"), " and ", tags$code("module_outputs"), " for data sharing."),
      tags$li("When adding cross-module behavior, centralize observers in orchestration layers instead of embedding ad hoc coupling inside module internals.")
    )
  )
}

#' Technical Documentation — Reactive data flow
#' @keywords internal
render_tech_miraprot_reactive_content <- function() {
    tags$div(
      class = "tech-doc",
      tags$h2("Reactive Data Flow and Module Orchestration"),
      tags$h3("Startup-to-reactive wiring (concise diagram)"),
      sp_doc_code_panel(paste(
        "app server boot",
        "  -> rv + module_outputs created",
        "  -> initialize_all_modules(...) registers producers/consumers",
        "  -> setup_reactive_coordination(...) wires shared buses + cross-module observers",
        "  -> setup_export_handlers(...) subscribes export consumers",
        "  -> user actions trigger producer updates (Data Wizard / GO / GSEA / etc.)",
        "  -> shared buses fan out updates to Volcano / Dot / Venn / Heatmap / STRING / export",
        sep = "\n"
      )),
    tags$h3("Startup sequence inside app server"),
      sp_doc_code_panel(paste(
        "rv <- reactiveValues()",
        "module_outputs <- reactiveValues()",
        "initialize_all_modules(module_outputs, rv, modEnv)",
        "coordination <- setup_reactive_coordination(input, output, rv, module_outputs, modEnv)",
        "setup_export_handlers(output, rv, module_outputs)",
        "setup_diagnostics(input, output, session, module_outputs, coordination$module_status, modEnv)",
        sep = "\n"
      )),
    tags$h3("Central reactive concepts"),
    tags$ul(
      tags$li(tags$b("rv"), ": mutable shared state for input data, derived data, and display metadata."),
      tags$li(tags$b("module_outputs"), ": module API registry returned by server module calls."),
      tags$li(tags$b("Reactive wrappers"), ": helper reactives in server_modules.R adapt GO/GSEA results for dependent modules."),
      tags$li(tags$b("Coordination layer"), ": server_coordination observers synchronize window-size inputs, GO propagation, and status tracking.")
    ),
    tags$h3("Reactive ownership map"),
    tags$ul(
      tags$li(tags$b("Producers"), ": Data Wizard ingestion/filters, GO enrichment outputs, GSEA outputs, and other analysis modules that publish computed reactives."),
      tags$li(tags$b("Shared buses"), ": ", tags$code("rv"), " holds mutable shared state; ", tags$code("module_outputs"), " holds module return objects/reactive accessors."),
      tags$li(tags$b("Consumers"), ": Volcano, Dot Plot, Venn, Heatmap, STRING, and export handlers subscribe to producer-derived values via wrappers and coordination observers."),
      tags$li("Ownership rule of thumb: producers publish, buses route, consumers read; avoid direct consumer-to-consumer dependency links.")
    ),
    tags$h3("Lifecycle and availability notes"),
    tags$ul(
      tags$li(tags$b("Initialization order"), ": create buses first (", tags$code("rv"), ", ", tags$code("module_outputs"), "), then initialize modules, then wire coordination/exports/diagnostics."),
      tags$li(tags$b("Late availability"), ": GO/GSEA and other compute-heavy outputs may be absent at startup and only appear after user-triggered runs."),
      tags$li(tags$b("Null-guard patterns"), ": use ", tags$code("req(...)"), ", ", tags$code("is.null(...)"), " checks, and fallback placeholders before dereferencing optional reactives."),
      tags$li("Guard coordination observers against partial startup states so they no-op safely until required producers publish values.")
    ),
    tags$h3("Safe extension checklist for new reactives"),
    tags$ul(
      tags$li(tags$b("Naming conventions"), ": use explicit, module-scoped names (e.g., ", tags$code("go_selected_terms"), ", ", tags$code("gsea_ranked_table"), ") and keep wrapper names aligned with producer intent."),
      tags$li(tags$b("Null-safe reads"), ": every cross-module read should be wrapped with ", tags$code("req"), " or explicit null checks; never assume producer completion during startup."),
      tags$li(tags$b("No circular observe chains"), ": keep dependency direction one-way (producer -> bus -> consumer) and avoid observers that write back into their own upstream triggers."),
      tags$li("Register new reactive contracts in orchestration layers so data flow remains discoverable and debuggable.")
    ),
    tags$h3("Reactive behavior relevant for maintenance"),
    tags$ul(
      tags$li("Module initialization is defensive; functions are called with and without debug_level for compatibility."),
      tags$li("Diagnostics perform one-shot startup checks for expected files and server functions."),
      tags$li("Session shutdown runs centralized cleanup and can stop the app via later::later(shiny::stopApp).")
    )
  )
}

#' Technical Documentation — Local setup and bootstrapping
#' @keywords internal
render_tech_miraprot_local_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("Local R-based Installation and Bootstrapping"),
    tags$h3("Execution pathway"),
    tags$ol(
      tags$li("bootstrap package loading: startup code in ", tags$code("R/bootstrap.R"), " configures package imports, options, portable-mode flags, and debug helpers before app-level orchestration runs."),
      tags$li("modEnv population: ", tags$code("app.R"), " creates/rebuilds ", tags$code("modEnv"), " and sources wrappers/infrastructure files so module entrypoints are discoverable."),
      tags$li("UI construction: ", tags$code("build_ui(modEnv)"), " resolves module UI wrappers and assembles the top-level navbar layout."),
      tags$li("module init: server startup calls ", tags$code("initialize_all_modules(module_outputs, rv, modEnv)"), " and records startup status with defensive function invocation."),
      tags$li("coordination/exports/diagnostics wiring: orchestration finalizes reactive coordination, export handlers, and diagnostics hooks before normal interaction begins."),
      tags$li("session$onSessionEnded triggers centralized cleanup and optional app stop.")
    ),
    div(
      class = "alert alert-secondary",
      tags$b("Installer resilience note"),
      tags$p(
        "The dependency scripts ",
        tags$code("install.R"),
        ", ",
        tags$code("update.R"),
        ", and ",
        tags$code("portable/scripts/install-packages.R"),
        " detect local build-tool availability before GitHub installs. If build tools are missing, they automatically fall back to CRAN installation for ",
        tags$code("shinyTree"),
        " to avoid hard failures on fresh systems."
      )
    ),
    tags$h3("If startup fails, inspect these files first"),
    tags$table(
      class = "table table-bordered table-sm",
      tags$thead(tags$tr(tags$th("File"), tags$th("Why inspect"), tags$th("Typical failure clues"))),
      tags$tbody(
        tags$tr(
          tags$td(tags$code("R/bootstrap.R")),
          tags$td("Verifies package loading, option defaults, debug utility setup, and startup-time environment flags."),
          tags$td("library()/require() errors, invalid options, debug/logging setup not initialized.")
        ),
        tags$tr(
          tags$td(tags$code("app.R")),
          tags$td("Controls top-level sourcing order, modEnv creation, UI build call, and main server wiring."),
          tags$td("source() failures, object-not-found during modEnv population, failures before server() fully registers.")
        ),
        tags$tr(
          tags$td(tags$code("R/server_modules.R")),
          tags$td("Defines module initialization contracts and compatibility wrappers around module server calls."),
          tags$td("Module init warnings/errors, missing return structures, compatibility fallback warnings.")
        ),
        tags$tr(
          tags$td(tags$code("modules/*_module.R")),
          tags$td("Provides wrapper-level UI/server entry functions that are loaded into modEnv."),
          tags$td("Missing/misnamed wrapper functions, syntax errors, server function absent in modEnv.")
        )
      )
    ),
    tags$h3("Using DEBUG_LEVEL and logging for triage"),
    tags$ul(
      tags$li("Set ", tags$code("DEBUG_LEVEL"), " before launch to increase startup verbosity and expose execution checkpoints."),
      tags$li("Use a low value for baseline startup confirmation; raise it when investigating sourcing and module init ordering problems."),
      tags$li("Correlate logs from bootstrap and module initialization paths to isolate whether failure happens before or after ", tags$code("initialize_all_modules"), "."),
      tags$li("When a failure is intermittent, compare consecutive startup logs to identify the first missing checkpoint or wrapper load event.")
    ),
    sp_doc_code_panel(paste(
      "# Example: raise startup verbosity in local shell",
      "Sys.setenv(DEBUG_LEVEL = '2')",
      "shiny::runApp('.')",
      sep = "\n"
    )),
    tags$h3("Common startup failure signatures"),
    tags$table(
      class = "table table-bordered table-sm",
      tags$thead(tags$tr(tags$th("Signature"), tags$th("Likely cause"), tags$th("First response"))),
      tags$tbody(
        tags$tr(
          tags$td("Missing package during bootstrap (e.g., 'there is no package called ...')"),
          tags$td("Required dependency is not installed in the active library path."),
          tags$td("Run ", tags$code("source('install.R')"), ". Installer scripts now run post-install verification and fail fast with an explicit missing-package list (including Bioconductor packages such as AnnotationHub) before app launch.")
        ),
        tags$tr(
          tags$td("BiocManager warns that CRAN replaces Bioconductor repos and AnnotationHub is source-only"),
          tags$td("Repository options were overridden or Bioconductor binaries are not yet available for the active R version/platform."),
          tags$td("Installer scripts now switch to ", tags$code("BiocManager::repositories()"), " for Bioconductor steps and emit a dedicated message when build tools are required for source-only Bioconductor packages.")
        ),
        tags$tr(
          tags$td("Missing module function in modEnv (object/function not found)"),
          tags$td("Wrapper file failed to source, function name mismatch, or wrapper not included in sourcing list."),
          tags$td("Check ", tags$code("modules/*_module.R"), " naming + syntax and confirm ", tags$code("app.R"), " source order.")
        ),
        tags$tr(
          tags$td("Module init warning from server_modules initialization path"),
          tags$td("Server wrapper contract mismatch, reactive input not yet available, or fallback invocation path triggered."),
          tags$td("Inspect initialization logs at higher ", tags$code("DEBUG_LEVEL"), " and validate module wrapper signatures.")
        )
      )
    ),
    tags$h3("Local developer workflow"),
    sp_doc_code_panel(paste(
      "# one-time dependency installation",
      "source('install.R')",
      "",
      "# run application",
      "shiny::runApp('.')",
      sep = "\n"
    )),
    tags$h3("Code paths involved"),
    tags$ul(
      tags$li(tags$code("app.R"), " and all files under ", tags$code("R/"), " for startup and orchestration."),
      tags$li(tags$code("modules/*.R"), " wrappers for module UI/server registration."),
      tags$li(tags$code("modules/<module_name>/"), " implementation internals by module."),
      tags$li(tags$code("Documentation/*.R"), " in-app documentation modules loaded into modEnv.")
    )
  )
}

#' Technical Documentation — Portable build, packaging, and runtime
#' @keywords internal
render_tech_miraprot_portable_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("Portable Build and Runtime Workflow"),
    tags$p("Portable mode combines the Shiny app with a Go launcher, bundled R runtime, package library, and optional prebuilt caches."),
    tags$h3("Key directories and scripts"),
    tags$ul(
      tags$li(tags$code("portable/launcher/"), ": launcher source code (CLI, process management, tray, lockfile, update check)."),
      tags$li(tags$code("portable/scripts/bundle-r.sh"), ": Linux/macOS bundler."),
      tags$li(tags$code("portable/scripts/bundle-r-windows.ps1"), ": Windows bundler."),
      tags$li(tags$code("portable/scripts/install-packages.R"), ": package installation into bundled library."),
      tags$li(tags$code("portable/scripts/prebuild-cache.R"), ": prebuild AnnotationHub and GO caches."),
      tags$li(tags$code("portable/installers/"), ": platform-specific installer builders (Inno Setup, DMG, AppImage).")
    ),
    tags$h3("Packaging/build flow"),
    tags$ol(
      tags$li("Prepare output directory (dist)."),
      tags$li("Bundle portable R into r-portable/."),
      tags$li("Install required packages into r-library/."),
      tags$li("Optional: prebuild go-cache for GO and AnnotationHub."),
      tags$li("Copy application source into shiny-app/."),
      tags$li("Build MiraProt-launcher with version metadata via go build."),
      tags$li("Optionally package dist into installer artifacts.")
    ),
    tags$h3("Portable runtime flow"),
    tags$ol(
      tags$li("Launcher acquires single-instance lock and validates paths."),
      tags$li("Launcher finds free port (3838-4838) and starts Rscript with shiny::runApp."),
      tags$li("Launcher polls the root app URL until HTTP 200, then opens browser unless disabled."),
      tags$li("Launcher injects runtime environment variables (R_LIBS_USER, MIRAPROT_IN_PORTABLE, cache and log paths)."),
      tags$li("Shiny app detects portable mode in bootstrap and enables portable settings."),
      tags$li("Shutdown occurs on tray quit, idle timeout, process signal, or R termination.")
    ),
    tags$h3("Local versus portable: technical differences"),
    tags$table(
      class = "table table-bordered table-sm",
      tags$thead(tags$tr(tags$th("Aspect"), tags$th("Local R-based"), tags$th("Portable"))),
      tags$tbody(
        tags$tr(tags$td("R runtime"), tags$td("System installation"), tags$td("Bundled in r-portable/")),
        tags$tr(tags$td("Package library"), tags$td("User or project libraries"), tags$td("Bundled r-library/ set via R_LIBS_USER")),
        tags$tr(tags$td("Startup command"), tags$td("shiny::runApp('.')"), tags$td("Go launcher starts Rscript --vanilla -e shiny::runApp")),
        tags$tr(tags$td("Process control"), tags$td("Managed by user or IDE"), tags$td("Launcher manages lifecycle, port selection, lockfile, tray")),
        tags$tr(tags$td("Caching"), tags$td("User-managed caches"), tags$td("go-cache plus launcher-seeded cache directories"))
      )
    ),
    div(
      class = "alert alert-info",
      tags$b("Important maintenance note"),
      tags$p("In launcher code, readiness polling checks the app root URL. The /__health endpoint is used by the idle monitor inside the launcher, not for startup readiness polling.")
    )
  )
}

#' Technical Documentation — Session save/restore and debug-level plumbing
#' @keywords internal
render_tech_miraprot_session_content <- function() {
  tags$div(
    class = "tech-doc",
    tags$h2("Session Tab: Save/Restore, Debug-Level, and Shutdown Plumbing"),
    tags$p(
      "The Session tab is the developer-facing control plane for snapshot persistence, restoration sequencing, and runtime debug-level control."
    ),
    tags$h3("UI contract and input/output IDs"),
    tags$ul(
      tags$li(tags$code("session_save_level"), ": save-level selector (data_only, data_and_analysis, full_session)."),
      tags$li(tags$code("session_download"), ": download handler for writing a snapshot .rds file."),
      tags$li(tags$code("session_file"), ": file input for selecting a snapshot to restore."),
      tags$li(tags$code("session_restore_status"), ": validation + restore status output."),
      tags$li(tags$code("session_deep_cleanup_shutdown"), ": action button for deep cleanup + app stop."),
      tags$li(tags$code("debug_level_select"), ": canonical debug-level selector (0/1/2)."),
      tags$li(tags$code("session_log_display"), ": session log output sourced from the currently selected per-level logger.")
    ),
    tags$h3("Snapshot structure and save levels"),
    tags$ul(
      tags$li("Snapshot payload includes shared ", tags$code("rv_snapshot"), ", metadata (created_at, app version, schema version), selected save level, and optional ", tags$code("module_snapshots"), "."),
      tags$li(tags$b("data_only"), ": prioritize processed data + metadata and Data Wizard pipeline state."),
      tags$li(tags$b("data_and_analysis"), ": includes analysis products such as GO/GSEA outputs."),
      tags$li(tags$b("full_session"), ": extends snapshot with visualization-module configuration state.")
    ),
    tags$p(
      "The save path degrades gracefully by dropping failing module snapshots when possible, and records exclusions in manifest metadata."
    ),
    tags$h3("Restore lifecycle"),
    tags$ol(
      tags$li("Validate uploaded snapshot and expose metadata summary to ", tags$code("session_restore_status"), "."),
      tags$li("Preload restored per-level debug-log buffers into the session log view."),
      tags$li("Apply saved debug level by updating ", tags$code("debug_level_select"), "."),
      tags$li("Set ", tags$code("rv$session_restoring <- TRUE"), " before mutating shared state."),
      tags$li("Restore shared ", tags$code("rv"), " fields (excluding ephemeral guards)."),
      tags$li("Restore module snapshots through the registry restore API with ordered progress callbacks."),
      tags$li("Clear restoration guard and bump ", tags$code("rv$session_restore_trigger"), " on ", tags$code("session$onFlushed"), " to avoid same-flush clobbering by downstream observers."),
      tags$li("Emit summary notification with restored-module counts and warning details.")
    ),
    tags$h3("Shutdown lifecycle and cleanup modes"),
    tags$ul(
      tags$li(tags$b("Default close path"), ": ", tags$code("session$onSessionEnded"),
              " triggers ", tags$code("cleanup_manager$execute_cleanup(mode = 'session_end')"),
              ", then optional ", tags$code("later::later(shiny::stopApp)"), " based on ",
              tags$code("getOption('miraprot.stop_on_close', TRUE)"), "."),
      tags$li(tags$b("Process stop path"), ": ", tags$code("shiny::onStop"),
              " runs a final cleanup pass via ",
              tags$code("cleanup_manager$execute_cleanup(mode = 'app_stop')"), "."),
      tags$li(tags$b("Deep cleanup button path"), ": Session tab button temporarily enables ",
              tags$code("miraprot.cleanup.close_connections = TRUE"), " and ",
              tags$code("miraprot.cleanup.run_gc = TRUE"),
              ", runs ", tags$code("execute_cleanup(mode = 'app_stop')"),
              ", removes the lockfile, and schedules ", tags$code("shiny::stopApp()"), ".")
    ),
    tags$p(
      "Deep cleanup button execution is wrapped in ",
      tags$code("withProgress"), " so users see shutdown phase feedback in the UI."
    ),
    tags$h3("Cleanup performance model"),
    tags$ul(
      tags$li("Centralized cleanup runs in three timed phases: parallel resources, reactive/module teardown, and system resources."),
      tags$li("Module cleanup callbacks are guarded with elapsed time limits to prevent indefinite shutdown hangs."),
      tags$li("Potentially blocking system tasks (connection closing and synchronous GC) are option-gated so normal shutdown remains fast by default.")
    ),
    div(
      class = "alert alert-secondary",
      tags$b("Concurrency guard rationale"),
      tags$p(
        "Guarded restore is mandatory because many observers read shared rv fields and can overwrite just-restored values if the guard is cleared too early."
      )
    ),
    tags$h3("Debug-level synchronization"),
    tags$ul(
      tags$li("Session-tab selector writes the canonical ", tags$code("DEBUG_LEVEL"), " binding in ", tags$code("globalenv()"), " via ", tags$code("assign(..., envir = globalenv())"), "."),
      tags$li("Input sanitization accepts integer values 0..2; invalid values are ignored."),
      tags$li("A no-op guard skips redundant writes and duplicate log lines when the selected level is unchanged (including restore-time ", tags$code("updateSelectInput"), " calls).")
    ),
    tags$h3("Session log rendering model"),
    tags$ul(
      tags$li(tags$code("reactivePoll"), " watches a lightweight global version counter instead of diffing full log payloads."),
      tags$li("Logging is stored in three dedicated FIFO buffers (levels 0, 1, and 2), each capped at 5000 entries."),
      tags$li("Changing ", tags$code("debug_level_select"), " switches the displayed logger to the matching cumulative buffer (0-only, 0+1, or 0+1+2)."),
      tags$li("This separation prevents verbose level-2 traffic from evicting critical level-0 history.")
    ),
    tags$h3("Files to inspect when debugging session restoration"),
    tags$ul(
      tags$li(tags$code("R/ui.R"), ": Session tab UI controls and labels."),
      tags$li(tags$code("R/session_save_restore/session_save_restore_orchestration.R"), ": save handler, validation, restore phases, log filtering, deep-shutdown button handler."),
      tags$li(tags$code("R/centralized_cleanup.R"), ": centralized cleanup manager modes, phase timing, and option-gated system cleanup."),
      tags$li(tags$code("app.R"), ": onSessionEnded/onStop wiring for cleanup mode execution."),
      tags$li(tags$code("R/server_coordination.R"), ": debug-level observer that updates global canonical state."),
      tags$li(tags$code("R/server_modules.R"), " and module-level restore hooks: guard-aware observers and module snapshot handlers.")
    )
  )
}
