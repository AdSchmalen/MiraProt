# R/server_modules.R
# ========================================
# Module Initialization Functions
# ========================================
# Called from the server function in app.R.
# Depends on: DEBUG_LEVEL, debug_log() from R/bootstrap.R
#             modEnv, rv, module_outputs from app.R server scope

# --- Reactive wrapper helpers (DRY) ---
# These create reactive expressions that safely extract results from
# GSEA/GO modules. Used by Volcano, Dotplot, PCA, Venn, Heatmap, STRING.

.gsea_results_reactive <- function(module_outputs) {
  # Use isolate() to safely read reactiveValues fields outside a reactive consumer
  if (!is.null(isolate(module_outputs$gsea_out))) {
    reactive({
      gsea_out <- module_outputs$gsea_out
      if ("has_results" %in% names(gsea_out) &&
          is.function(gsea_out$has_results) &&
          !isTRUE(gsea_out$has_results())) {
        return(NULL)
      }
      if ("get_results" %in% names(gsea_out)) {
        gsea_out$get_results()
      } else NULL
    })
  } else NULL
}

.go_results_reactive <- function(module_outputs) {
  # Use isolate() to safely read reactiveValues fields outside a reactive consumer
  if (!is.null(isolate(module_outputs$go_out))) {
    reactive({
      go_out <- module_outputs$go_out
      if ("has_results" %in% names(go_out) &&
          is.function(go_out$has_results) &&
          !isTRUE(go_out$has_results())) {
        return(NULL)
      }
      if ("get_results" %in% names(go_out)) {
        go_out$get_results()
      } else NULL
    })
  } else NULL
}

# --- Safe module initialization helper ---
# Checks the module server signature before passing optional parameters.
# Shows user notification only for critical modules (Data Wizard).

.module_debug_level_compat_logged <- new.env(parent = emptyenv())

initialize_module_safely <- function(module_name, module_function_name, module_id, rv,
                                     debug_level = DEBUG_LEVEL, additional_params = NULL) {
  tryCatch({
    if (exists(module_function_name, envir = modEnv)) {
      module_func <- get(module_function_name, envir = modEnv)
      module_formals <- names(formals(module_func))
      supports_debug_level <- "debug_level" %in% module_formals

      args <- list(id = module_id, rv = rv)
      if (supports_debug_level) {
        args$debug_level <- debug_level
      } else {
        compat_key <- module_function_name
        if (!isTRUE(.module_debug_level_compat_logged[[compat_key]])) {
          debug_log(paste(module_name, "module server does not declare debug_level; initializing without it"), 2)
          .module_debug_level_compat_logged[[compat_key]] <- TRUE
        }
      }

      if (!is.null(additional_params)) {
        args <- c(args, additional_params)
      }

      result <- do.call(module_func, args)

      return(result)

    } else {
      stop(paste(module_function_name, "function not found in modEnv"))
    }

  }, error = function(e) {
    debug_log(paste("Error initializing", module_name, "module:", e$message), 1)

    if (module_name == "Data Wizard") {
      showNotification(
        paste(module_name, "module failed to initialize."),
        type = "warning",
        duration = 8
      )
    }

    return(NULL)
  })
}

# --- Initialize integration module (Volcano, Dotplot, PCA, Venn, Heatmap) ---
# Common pattern: module server with GSEA/GO reactive wrappers.

.init_integration_module <- function(module_name, server_func, module_id,
                                     rv, module_outputs, modEnv,
                                     gsea_param = "res_GSEA", go_param = "GO_res",
                                     extra_params = list()) {
  tryCatch({
    args <- c(
      list(
        id = module_id,
        rv = rv
      ),
      stats::setNames(list(.gsea_results_reactive(module_outputs)), gsea_param),
      stats::setNames(list(.go_results_reactive(module_outputs)), go_param),
      list(
        module_outputs = module_outputs,
        debug_level = DEBUG_LEVEL
      ),
      extra_params
    )

    do.call(server_func, args)
  }, error = function(e) {
    debug_log(paste("Error initializing", module_name, "module:", e$message), 1)
    showNotification(
      paste(module_name, "module failed to load:", e$message),
      type = "warning",
      duration = 8
    )
    NULL
  })
}

# --- Grid module initialization (triple-fallback) ---

.init_grid_module <- function(rv, modEnv, module_outputs = NULL) {
  tryCatch({
    if (exists("modGridServer", envir = modEnv)) {
      result <- tryCatch({
        modEnv$modGridServer("grid", rv, module_outputs = module_outputs)
      }, error = function(e1) {
        debug_log(paste("Grid module simple init failed:", e1$message), 2)
        tryCatch({
          modEnv$modGridServer("grid", rv, module_outputs = module_outputs, debug_level = DEBUG_LEVEL)
        }, error = function(e2) {
          debug_log(paste("Grid module with debug_level failed:", e2$message), 2)
          modEnv$modGridServer(id = "grid", rv = rv, module_outputs = module_outputs)
        })
      })

      if (!is.null(result)) {
        result
      } else {
        debug_log("Grid module returned NULL, marking as initialized for status display", 1)
        list(initialized = TRUE)
      }

    } else {
      debug_log("modGridServer function not found in modEnv", 1)
      if (file.exists("modules/Grid_module.R")) {
        debug_log("Attempting to source Grid module file", 2)
        source("modules/Grid_module.R", local = modEnv)
        if (exists("modGridServer", envir = modEnv)) {
          debug_log("Grid module function found after sourcing", 2)
          modEnv$modGridServer("grid", rv, module_outputs = module_outputs)
        } else {
          debug_log("Grid module function still not found after sourcing", 1)
          NULL
        }
      } else {
        debug_log("Grid module file not found: modules/Grid_module.R", 1)
        NULL
      }
    }

  }, error = function(e) {
    debug_log(paste("Error initializing Grid module:", e$message), 1)
    showNotification(
      paste("Plot Grid module failed to load:", e$message),
      type = "warning",
      duration = 8
    )
    NULL
  })
}

# --- STRING module initialization (quad-fallback) ---

.init_string_module <- function(rv, module_outputs, modEnv) {
  tryCatch({
    if (exists("modSTRINGServer", envir = modEnv)) {
      result <- tryCatch({
        modEnv$modSTRINGServer(
          id = "string",
          rv = rv,
          res_GSEA = .gsea_results_reactive(module_outputs),
          GO_res = .go_results_reactive(module_outputs),
          module_outputs = module_outputs,
          debug_level = DEBUG_LEVEL
        )
      }, error = function(e1) {
        debug_log(paste("STRING full init failed:", e1$message), 2)
        tryCatch({
          modEnv$modSTRINGServer(id = "string", rv = rv, debug_level = DEBUG_LEVEL)
        }, error = function(e2) {
          debug_log(paste("STRING simplified init failed:", e2$message), 2)
          tryCatch({
            modEnv$modSTRINGServer("string", rv)
          }, error = function(e3) {
            debug_log(paste("STRING basic init failed:", e3$message), 2)
            tryCatch({
              func_formals <- formals(modEnv$modSTRINGServer)
              debug_log(paste("STRING function parameters:", paste(names(func_formals), collapse = ", ")), 2)
              if ("module_outputs" %in% names(func_formals)) {
                modEnv$modSTRINGServer(id = "string", rv = rv, module_outputs = module_outputs)
              } else {
                modEnv$modSTRINGServer(id = "string", rv = rv)
              }
            }, error = function(e4) {
              debug_log(paste("All STRING init approaches failed:", e4$message), 1)
              NULL
            })
          })
        })
      })

      if (!is.null(result)) {
        result
      } else {
        debug_log("STRING module returned NULL, marking as initialized for status display", 1)
        list(initialized = TRUE)
      }

    } else {
      debug_log("modSTRINGServer function not found in modEnv", 1)
      if (file.exists("modules/STRING_module.R")) {
        debug_log("Attempting to source STRING module file", 2)
        source("modules/STRING_module.R", local = modEnv)
        if (exists("modSTRINGServer", envir = modEnv)) {
          debug_log("STRING module function found after sourcing", 2)
          modEnv$modSTRINGServer("string", rv)
        } else {
          debug_log("STRING module function still not found after sourcing", 1)
          NULL
        }
      } else {
        debug_log("STRING module file not found: modules/STRING_module.R", 1)
        NULL
      }
    }

  }, error = function(e) {
    debug_log(paste("Error initializing STRING DB module:", e$message), 1)
    showNotification(
      paste("STRING DB module failed to load:", e$message),
      type = "warning",
      duration = 8
    )
    NULL
  })
}

# ========================================
# Main Entry Point: Initialize All Modules
# ========================================
# Called once from app.R server function.
# Populates module_outputs, sets up modEnv compat copies,
# initializes doc modules, and creates data sync observer.

initialize_all_modules <- function(module_outputs, rv, modEnv) {

  # --- Core modules (simple init via helper) ---
  module_outputs$datawizard_out <- initialize_module_safely("Data Wizard", "modDataWizardServer", "datawizard", rv)
  module_outputs$modtable_out   <- initialize_module_safely("Modified Table", "modModifiedTableServer", "modtable", rv)
  module_outputs$abundance_out  <- initialize_module_safely("Abundance", "modAbundancesServer", "abundance", rv)
  module_outputs$sampleid_out   <- initialize_module_safely("Sample IDs", "modSampleIDsServer", "sampleid", rv)
  module_outputs$go_out         <- initialize_module_safely("GO", "modGOServer", "go", rv)

  # --- GSEA module (custom init, needs modEnv parameter) ---
  module_outputs$gsea_out <- tryCatch({
    if (exists("GSEA_module_server", envir = modEnv)) {
      modEnv$GSEA_module_server(
        id = "gsea",
        rv = rv,
        debug_level = DEBUG_LEVEL,
        modEnv = modEnv
      )
    } else {
      debug_log("GSEA_module_server not found in modEnv", 1)
      showNotification("GSEA Analysis module could not be loaded.", type = "warning", duration = 8)
      NULL
    }
  }, error = function(e) {
    debug_log(paste("Error initializing GSEA Analysis module:", e$message), 1)
    showNotification(paste("GSEA Analysis module failed to load:", e$message), type = "warning", duration = 8)
    NULL
  })

  # --- Integration modules (GSEA/GO reactive wrappers) ---
  module_outputs$volcano_out <- .init_integration_module(
    "Volcano Plot", modEnv$modVolcanoServer, "volcano",
    rv, module_outputs, modEnv, extra_params = list(modEnv = modEnv)
  )

  module_outputs$dotplot_out <- .init_integration_module(
    "Dotplot", modEnv$modDotPlotServer, "dotplot",
    rv, module_outputs, modEnv,
    gsea_param = "res_GSEA", go_param = "res_GO",
    extra_params = list(modEnv = modEnv)
  )

  module_outputs$pca_out <- .init_integration_module(
    "PCA", modEnv$modPCAServer, "pca",
    rv, module_outputs, modEnv,
    gsea_param = "res_GSEA", go_param = "res_GO",
    extra_params = list(modEnv = modEnv)
  )

  module_outputs$venn_out <- .init_integration_module(
    "Venn Analysis", modEnv$modVennServer, "venn",
    rv, module_outputs, modEnv,
    extra_params = list()
  )

  module_outputs$heatmap_out <- .init_integration_module(
    "Enhanced Heatmap", modEnv$modHeatmapServer, "heatmap",
    rv, module_outputs, modEnv, extra_params = list(modEnv = modEnv)
  )

  # --- Grid module (triple-fallback) ---
  module_outputs$grid_out <- .init_grid_module(rv, modEnv, module_outputs)

  # --- STRING module (quad-fallback) ---
  module_outputs$STRING_out <- .init_string_module(rv, module_outputs, modEnv)

  # --- Backward-compatibility: store module outputs in modEnv ---
  tryCatch({
    isolate({
      modEnv$datawizard_out <- module_outputs$datawizard_out
      modEnv$gsea_out       <- module_outputs$gsea_out
      modEnv$pca_out        <- module_outputs$pca_out
      modEnv$volcano_out    <- module_outputs$volcano_out
      modEnv$go_out         <- module_outputs$go_out
      modEnv$heatmap_out    <- module_outputs$heatmap_out
      modEnv$STRING_out     <- module_outputs$STRING_out
      modEnv$venn_out       <- module_outputs$venn_out
    })

    # Data sync observer: keep rv in sync with datawizard output
    # Skipped during session restoration to prevent overwriting restored data
    observe({
      # Guard: skip during session restoration
      if (isTRUE(rv$session_restoring)) return()

      if (!is.null(module_outputs$datawizard_out) &&
          is.list(module_outputs$datawizard_out) &&
          "final_processed_data" %in% names(module_outputs$datawizard_out) &&
          "final_processed_metadata" %in% names(module_outputs$datawizard_out)) {

        tryCatch({
          initial_data <- module_outputs$datawizard_out$final_processed_data()
          initial_metadata <- module_outputs$datawizard_out$final_processed_metadata()

          if (!is.null(initial_data)) {
            rv$data_mod <- initial_data
            rv$data_def <- initial_metadata
          }
        }, error = function(e) {
          debug_log(paste("Error in initial data sync:", e$message), 2)
        })
      }
    })

  }, error = function(e) {
    debug_log(paste("Error in module setup:", e$message), 2)
  })

  # --- Documentation modules ---
  # Guard every call: if a doc file failed to load the function will be absent
  # from modEnv and calling NULL() would crash the whole app.
  .init_doc_module <- function(fn_name, module_id) {
    fn <- modEnv[[fn_name]]
    if (is.function(fn)) {
      tryCatch(
        fn(module_id, debug_level = DEBUG_LEVEL),
        error = function(e) {
          debug_log(paste("[DOC]", fn_name, "error:", e$message), 1)
        }
      )
    } else {
      debug_log(paste("[DOC]", fn_name, "not found in modEnv; skipping"), 1)
    }
  }

  .init_doc_module("modMiraProtDocServer",    "miraprot_doc")
  .init_doc_module("modDatawizardDocServer",   "datawizard_doc")
  .init_doc_module("modTablesModifiedDocServer", "tables_modified_doc")
  .init_doc_module("modAbundancesDocServer",   "abundances_doc")
  .init_doc_module("modSampleIDsDocServer",    "sampleids_doc")
  .init_doc_module("modDimRedDocServer",       "pca_doc")
  .init_doc_module("modVennDocServer",         "venn_doc")
  .init_doc_module("modVolcanoDocServer",      "volcano_doc")
  .init_doc_module("modDotplotDocServer",      "dotplot_doc")
  .init_doc_module("modGODocServer",           "GO_doc")
  .init_doc_module("modGSEADocServer",         "GSEA_doc")
  .init_doc_module("modSTRINGDocServer",       "STRING_doc")
  .init_doc_module("modHeatmapDocServer",      "heatmap_doc")
  .init_doc_module("modGridDocServer",         "grid_doc")
}
