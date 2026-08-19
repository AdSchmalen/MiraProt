# ==============================================================================
# 12. RDS Import Observer and Status Output
# ==============================================================================

output$go_import_rds_status <- renderUI({
  status <- import_status_message()
  if (!nzchar(status)) return(NULL)
  div(
    style = "font-size: 0.9em; padding: 8px; background-color: #f8f9fa; border-radius: 3px;",
    status
  )
})

observeEvent(input$go_import_rds, {
  req(input$go_import_rds)
  fpath <- input$go_import_rds$datapath
  import_status_message("Importing GO RDS...")
  tryCatch({
    imported <- readRDS(fpath)

    if (!is.list(imported) || is.null(imported$Edo_GO)) {
      stop("Invalid GO RDS file: missing Edo_GO")
    }

    n_terms_imported <- tryCatch(
      nrow(as.data.frame(imported$Edo_GO@result)),
      error = function(e) NA_integer_
    )

    GO_Result_List(imported)
    go_results_ready_for_fallback(valid_go_results_available())
    debug_log(
      sprintf(
        "GO result imported | File: %s | Terms imported: %s",
        input$go_import_rds$name,
        as.character(n_terms_imported)
      ),
      level = 0
    )

    assign("GO_Result_List", function() GO_Result_List(), envir = modEnv)
    debug_log("GO_Result_List assigned to modEnv for Excel export", 1)

    tree_structure <- tryCatch({
      create_go_tree_structure(
        edo       = imported$Edo_GO,
        debug_log = debug_log
      )
    }, error = function(e) {
      debug_log(paste("Tree creation failed after RDS import:", e$message), 1)
      NULL
    })

    go_tree_structure(tree_structure)
    rv$go_results        <- imported
    rv$go_tree           <- tree_structure
    rv$go_selected_terms <- character(0)
    go_analysis_status("completed")

    import_status_message("✓ GO results imported from RDS")
    showNotification("GO results imported successfully from RDS", type = "message", duration = 4)
  }, error = function(e) {
    msg <- paste("✗ Import error:", e$message)
    import_status_message(msg)
    debug_log(msg, 1)
    showNotification(msg, type = "error", duration = 6)
  })
})

# ==============================================================================
# 13. Module Lifecycle
# ==============================================================================

shiny::onStop(function() {
  debug_log("DOWNLOAD: GO module stopped", 2)
})

observeEvent(TRUE, {
  initialize_download_dimensions()
}, once = TRUE)
