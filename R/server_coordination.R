# R/server_coordination.R
# ========================================
# Reactive Coordination: Cross-Module Observers
# ========================================
# Sets up reactive observers that coordinate data flow between modules.
# Called from the server function in app.R.
# Depends on: DEBUG_LEVEL, debug_log() from R/bootstrap.R
#             px2inch() from R/utils.R

# Returns a list with:
#   module_status  - reactive tracking module init success/failure
#                    (used by diagnostics in app.R)

setup_reactive_coordination <- function(input, output, rv, module_outputs, modEnv) {

  # ========================================
  # Window Size Tracking
  # ========================================

  output$window_size <- renderText({
    req(input$window_inner_width, input$window_inner_height, input$devicePixelRatio)

    width_px  <- input$window_inner_width
    height_px <- input$window_inner_height

    width_in  <- round(width_px  / 96, 2)
    height_in <- round(height_px / 96, 2)

    paste(
      "Window (px): ", width_px,  " \u00d7 ", height_px,
      " | Window (in): ", width_in, " \u00d7 ", height_in
    )
  })

  window_width  <- reactiveVal(NULL)
  window_height <- reactiveVal(NULL)

  observe({
    req(input$window_inner_width, input$window_inner_height, input$devicePixelRatio)
    width_in  <- px2inch(input$window_inner_width,  input$devicePixelRatio, scale = 0.75, dpi = 96)
    height_in <- px2inch(input$window_inner_height, input$devicePixelRatio, scale = 1,    dpi = 96)

    rv$px_ratio    <- input$devicePixelRatio
    rv$width_px    <- input$window_inner_width
    rv$width_inch  <- width_in
    rv$height_px   <- input$window_inner_height
    rv$height_inch <- height_in

    window_width(width_in)
    window_height(height_in)
  })

  # ========================================
  # GO Results Synchronization
  # ========================================

  # Sync GO module results -> rv$go_result (for cross-module access)
  # Skipped during session restoration to prevent overwriting restored data
  observe({
    if (isTRUE(rv$session_restoring)) return()

    if (!is.null(module_outputs$go_out)) {
      tryCatch({
        rv$go_result <- module_outputs$go_out$get_results_dataframe()
      }, error = function(e) {
        debug_log(paste("Error updating rv from go_out:", e$message), 2)
      })
    }
  })

  # Store GO results in modEnv for Excel export access
  observe({
    if (isTRUE(rv$session_restoring)) return()

    tryCatch({
      if (!is.null(module_outputs$go_out) &&
          "get_results" %in% names(module_outputs$go_out) &&
          "has_results" %in% names(module_outputs$go_out)) {

        has_results <- module_outputs$go_out$has_results()

        if (has_results) {
          go_results <- module_outputs$go_out$get_results()

          if (!is.null(go_results)) {
            assign("GO_Result_List", function() go_results, envir = modEnv)
          }
        }
      }
    }, error = function(e) {
      debug_log(paste("Error storing GO results in modEnv:", e$message), 1)
    })
  })

  # ========================================
  # Debug Level Control (Session tab)
  # ========================================

  # Update the canonical globalenv() DEBUG_LEVEL binding in response to the
  # Session tab's debug level selector.  Uses assign(..., envir = globalenv())
  # rather than <<- so the write always lands in the correct environment
  # regardless of what the Shiny support env may be shadowing.
  # The no-op guard prevents a redundant write + log line when restore calls
  # updateSelectInput() with the same value that is already active.
  observeEvent(input$debug_level_select, {
    new_level <- suppressWarnings(as.integer(input$debug_level_select))
    if (is.na(new_level) || !(new_level %in% 0:2)) return()
    cur <- get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE)
    if (identical(as.integer(cur), new_level)) return()
    assign("DEBUG_LEVEL", new_level, envir = globalenv())
    debug_log(paste("Debug level changed to", new_level), 1)
  }, ignoreInit = TRUE)

  # ========================================
  # Module Status Tracking
  # ========================================

  module_status <- reactive({
    modules_list <- isolate(reactiveValuesToList(module_outputs))
    successful_modules <- sum(!sapply(modules_list, is.null))
    total_modules <- length(modules_list)

    list(
      successful = successful_modules,
      total = total_modules,
      failed_modules = names(modules_list)[sapply(modules_list, is.null)]
    )
  })

  # Log module initialization results
  observe({
    status <- module_status()

    if (status$successful == status$total) {
      debug_log(paste("All", status$total, "modules loaded successfully"), 1)
      showNotification(
        "All modules loaded successfully.",
        type = "message",
        duration = 5
      )
    } else {
      debug_log(paste("Partial module load:", status$successful, "of", status$total, "modules loaded"), 1)

      if (length(status$failed_modules) > 0) {
        debug_log(paste("Failed modules:", paste(status$failed_modules, collapse = ", ")), 1)

        for (failed_module in status$failed_modules) {
          if (failed_module == "pca_out") {
            debug_log("PCA module failed - check if modules/PCA_module.R exists and is sourced", 1)
          }
          if (failed_module == "STRING_out") {
            debug_log("STRING module failed - check if modules/STRING_module.R exists and is sourced", 1)
          }
          if (failed_module == "heatmap_out") {
            debug_log("Heatmap module failed - check if modules/Heatmap_module.R exists and is sourced", 1)
          }
        }
      }
    }
  })

  # Return module_status for use by diagnostics (WP7)
  list(
    module_status = module_status
  )
}
