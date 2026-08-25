# R/server_diagnostics.R
# ========================================
# Diagnostics: Module Status, Debug Info, Health Checks
# ========================================
# Sets up diagnostic outputs and startup health checks.
# Called from the server function in app.R.
# Depends on: DEBUG_LEVEL, debug_log() from R/bootstrap.R

setup_diagnostics <- function(input, output, session, module_outputs, module_status, modEnv) {

  # ========================================
  # Debug: Module Availability Output
  # ========================================

  output$debug_module_status <- renderText({
    if (DEBUG_LEVEL >= 2) {
      module_details <- character()

      expected_modules <- c(
        "datawizard_out", "modtable_out", "abundance_out", "sampleid_out",
        "go_out", "grid_out", "gsea_out", "volcano_out", "dotplot_out",
        "pca_out", "venn_out", "STRING_out", "heatmap_out"
      )

      for (name in expected_modules) {
        status <- if (is.null(module_outputs[[name]])) "NULL" else "Available"
        module_details <- c(module_details, paste(name, ":", status))
      }

      # Check for unexpected modules
      actual_modules <- names(module_outputs)
      unexpected <- setdiff(actual_modules, expected_modules)
      if (length(unexpected) > 0) {
        module_details <- c(module_details, paste("UNEXPECTED:", paste(unexpected, collapse = ", ")))
      }

      paste("Module Status:", paste(module_details, collapse = " | "))
    } else {
      ""
    }
  })

  # ========================================
  # Detailed Module Status Output
  # ========================================

  output$module_status_detailed <- renderText({
    status <- module_status()

    status_lines <- character()

    module_definitions <- list(
      list(key = "datawizard_out", label = "Data Wizard"),
      list(key = "modtable_out", label = "Table"),
      list(key = "abundance_out", label = "Abundances"),
      list(key = "sampleid_out", label = "Sample IDs"),
      list(key = "pca_out", label = "Dimensionality Reduction"),
      list(key = "volcano_out", label = "Volcano Plot"),
      list(key = "dotplot_out", label = "Dot Plot"),
      list(key = "go_out", label = "GO"),
      list(key = "gsea_out", label = "GSEA"),
      list(key = "STRING_out", label = "STRING DB"),
      list(key = "venn_out", label = "Venn"),
      list(key = "heatmap_out", label = "Heatmap"),
      list(key = "grid_out", label = "Plot Grid")
    )

    for (i in seq_along(module_definitions)) {
      module_key <- module_definitions[[i]]$key
      module_label <- module_definitions[[i]]$label

      is_loaded <- !is.null(module_outputs[[module_key]])
      status_text <- if (is_loaded) "[OK] Loaded" else "[--] Not loaded"

      status_lines <- c(status_lines, paste(paste0(i, "."), module_label, ":", status_text))
    }

    summary_line <- "=== MODULE STATUS SUMMARY ==="
    summary_count <- paste("Total:", status$successful, "of", status$total, "modules loaded")

    if (status$successful < status$total) {
      failed_count <- paste("Failed modules:", paste(status$failed_modules, collapse = ", "))
      paste(c(summary_line, summary_count, failed_count, "", status_lines), collapse = "\n")
    } else {
      success_msg <- "ALL MODULES LOADED SUCCESSFULLY"
      paste(c(summary_line, summary_count, success_msg, "", status_lines), collapse = "\n")
    }
  })

  # ========================================
  # System Debug Info Output
  # ========================================

  output$debug_info <- renderText({
    invalidateLater(1000, session)
    input$debug_level_select
    current_debug_level <- get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE, ifnotfound = 0L)
    current_session_token <- tryCatch({
      restored <- session$userData$restored_session_token
      if (is.character(restored) && length(restored) == 1L && nzchar(restored)) restored else session$token
    }, error = function(e) session$token)

    info <- paste(
      "Debug Level:", current_debug_level,
      "\nMiraProt Version:", readLines("VERSION", n = 1L, warn = FALSE),
      "\nShiny Version:", as.character(utils::packageVersion("shiny")),
      "\nSession ID:", current_session_token,
      "\nR Version:", R.version.string,
      "\nTimestamp:", Sys.time()
    )
    if (MIRAPROT_IN_PORTABLE) {
      info <- paste(info,
        "\n--- Portable Mode ---",
        "\nPort:", Sys.getenv("MIRAPROT_PORT", "?"),
        "\nGO Cache:", Sys.getenv("MIRAPROT_GO_CACHE", "?"),
        "\nAnnotation Cache:", Sys.getenv("ANNOTATION_HUB_CACHE", "?"),
        "\nLog Dir:", Sys.getenv("MIRAPROT_LOG_DIR", "?")
      )
    }
    info
  })

  # ========================================
  # Startup Health Checks (run once)
  # ========================================

  # --- Check module prerequisites (server functions in modEnv) ---

  check_module_prerequisites <- function() {
    required_functions <- list(
      "datawizard_out" = "modDataWizardServer",
      "modtable_out"   = "modModifiedTableServer",
      "abundance_out"  = "modAbundancesServer",
      "sampleid_out"   = "modSampleIDsServer",
      "go_out"         = "modGOServer",
      "grid_out"       = "modGridServer",
      "gsea_out"       = "GSEA_module_server",
      "volcano_out"    = "modVolcanoServer",
      "dotplot_out"    = "modDotPlotServer",
      "pca_out"        = "modPCAServer",
      "venn_out"       = "modVennServer",
      "STRING_out"     = "modSTRINGServer",
      "heatmap_out"    = "modHeatmapServer"
    )

    missing_functions <- character()

    for (module_name in names(required_functions)) {
      func_name <- required_functions[[module_name]]
      if (!exists(func_name, envir = modEnv)) {
        missing_functions <- c(missing_functions, paste(module_name, "->", func_name))
        debug_log(paste("MISSING FUNCTION:", func_name, "for module", module_name), 1)
      }
    }

    if (length(missing_functions) > 0) {
      debug_log(paste("Missing server functions:", paste(missing_functions, collapse = ", ")), 1)
      list(success = FALSE, missing = missing_functions)
    } else {
      list(success = TRUE, missing = character())
    }
  }

  diagnostics_run <- reactiveVal(FALSE)

  observe({
    if (!diagnostics_run()) {
      prereq_check <- check_module_prerequisites()

      if (!prereq_check$success) {
        debug_log("=== MISSING FUNCTIONS DETECTED ===", 1)

        # Map function names to module file hints
        file_hints <- list(
          "modPCAServer"       = "modules/PCA_module.R",
          "GSEA_module_server" = "modules/GSEA_module.R",
          "modSTRINGServer"    = "modules/STRING_module.R",
          "modHeatmapServer"   = "modules/Heatmap_module.R",
          "modGridServer"      = "modules/Grid_module.R"
        )

        for (missing in prereq_check$missing) {
          debug_log(paste("Missing:", missing), 1)
          for (func_pattern in names(file_hints)) {
            if (grepl(func_pattern, missing)) {
              debug_log(paste0("SOLUTION: Check if ", file_hints[[func_pattern]], " is sourced correctly"), 1)
              debug_log(paste0("Try: source('", file_hints[[func_pattern]], "', local = modEnv)"), 1)
            }
          }
        }

        showNotification(
          paste("Module loading issues detected:", length(prereq_check$missing), "missing functions. Check console for details."),
          type = "warning",
          duration = 10
        )
      }

      diagnostics_run(TRUE)
    }
  })

  # --- Check module file existence ---

  check_module_files <- function() {
    module_files <- c(
      "modules/datawizard_module.R",
      "modules/modified_table_module.R",
      "modules/abundances_module.R",
      "modules/sampleids_module.R",
      "modules/GO_module.R",
      "modules/Grid_module.R",
      "modules/GSEA_module.R",
      "modules/volcano_module.R",
      "modules/dotplot_module.R",
      "modules/pca_module.R",
      "modules/Venn_module.R",
      "modules/STRING_module.R",
      "modules/Heatmap_module.R"
    )

    missing_files <- character()

    for (fp in module_files) {
      if (!file.exists(fp)) {
        missing_files <- c(missing_files, fp)
        debug_log(paste("MISSING FILE:", fp), 1)
      }
    }

    if (length(missing_files) > 0) {
      debug_log(paste("Missing module files:", paste(missing_files, collapse = ", ")), 1)
      list(success = FALSE, missing_files = missing_files)
    } else {
      list(success = TRUE, missing_files = character())
    }
  }

  file_check_run <- reactiveVal(FALSE)

  observe({
    if (!file_check_run()) {
      file_check <- check_module_files()

      if (!file_check$success) {
        debug_log("=== MISSING MODULE FILES DETECTED ===", 1)
        debug_log(paste("Missing files:", paste(file_check$missing_files, collapse = ", ")), 1)

        showNotification(
          paste("Missing module files detected:", length(file_check$missing_files), "files not found. Check console for details."),
          type = "error",
          duration = 15
        )
      }

      file_check_run(TRUE)
    }
  })

  invisible(NULL)
}
