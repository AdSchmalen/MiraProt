# R/centralized_cleanup.R
CentralCleanupManager <- R6::R6Class("CentralCleanupManager",
                                     public = list(
                                       initialize = function(debug_level = 1) {
                                         private$debug_level <- debug_level
                                         private$cleanup_functions <- list()
                                         private$cleanup_executed <- FALSE
                                       },

                                       # Internal logging function (no dependency on module debug_log)
                                       log = function(message, level = 1) {
                                         if (private$debug_level >= level) {
                                           timestamp <- format(Sys.time(), "%H:%M:%S")
                                           cat("[ CLEANUP_MANAGER", timestamp, "]", message, "\n")
                                         }
                                       },

                                       register_module = function(module_name, cleanup_function) {
                                         self$log(paste("Registering cleanup for module:", module_name), 2)
                                         private$cleanup_functions[[module_name]] <- cleanup_function
                                       },

                                       reset_shutdown_guard = function() {
                                         private$cleanup_executed <- FALSE
                                       },

                                       execute_cleanup = function(mode = c("session_end", "app_stop")) {
                                         mode <- match.arg(mode)

                                         if (isTRUE(private$cleanup_executed)) {
                                           self$log(sprintf("Centralized cleanup (%s) already executed", mode), 1)
                                           return(invisible(FALSE))
                                         }

                                         private$cleanup_executed <- TRUE
                                         self$log("Starting centralized cleanup", 1)
                                         t0 <- proc.time()[3]

                                         # 1. Stop parallel clusters FIRST
                                         t_step <- proc.time()[3]
                                         self$cleanup_parallel_resources()
                                         self$log(sprintf("  parallel resources: %dms",
                                                         round((proc.time()[3] - t_step) * 1000)), 1)

                                         # 2. Execute module-specific cleanup
                                         t_step <- proc.time()[3]
                                         self$cleanup_reactive_values()
                                         self$log(sprintf("  reactive values: %dms",
                                                         round((proc.time()[3] - t_step) * 1000)), 1)

                                         # 3. System-level cleanup
                                         t_step <- proc.time()[3]
                                         self$cleanup_system_resources(mode = mode)
                                         self$log(sprintf("  system resources: %dms",
                                                         round((proc.time()[3] - t_step) * 1000)), 1)

                                         total_ms <- round((proc.time()[3] - t0) * 1000)
                                         self$log(sprintf("Centralized cleanup completed in %dms", total_ms), 1)

                                         # Release references captured by old sessions to avoid
                                         # long-tail memory growth across repeated app restarts.
                                         private$cleanup_functions <- list()

                                         # Final step: reset global debug level for the next session.
                                         if (exists("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE)) {
                                           assign("DEBUG_LEVEL", 0L, envir = globalenv())
                                         }
                                       },

                                       cleanup_parallel_resources = function() {
                                         self$log("Cleaning up parallel resources", 2)

                                         # Keep cleanup lightweight and deterministic.
                                         if (tryCatch(foreach::getDoParRegistered(), error = function(e) FALSE)) {
                                           tryCatch({
                                             foreach::registerDoSEQ()
                                           }, error = function(e) {
                                             self$log(paste("Error resetting foreach backend:", e$message), 1)
                                           })
                                         }

                                         if ("doParallel" %in% loadedNamespaces()) {
                                           tryCatch(doParallel::stopImplicitCluster(), silent = TRUE)
                                         }

                                       },

                                       cleanup_reactive_values = function() {
                                         self$log("Cleaning up reactive values", 2)

                                         run_module_cleanup <- function(cleanup_fn) {
                                           setTimeLimit(cpu = Inf, elapsed = 5, transient = TRUE)
                                           on.exit(
                                             setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE),
                                             add = FALSE
                                           )
                                           cleanup_fn()
                                         }

                                         # Execute module cleanup functions
                                         for (module_name in names(private$cleanup_functions)) {
                                           tryCatch({
                                             self$log(paste("Cleaning up module:", module_name), 2)
                                             module_t0 <- proc.time()[3]

                                             # Guard against modules that hang during teardown.
                                             # Keep timeout generous to avoid false positives while
                                             # still preventing multi-minute shutdown stalls.
                                             run_module_cleanup(private$cleanup_functions[[module_name]])

                                             module_ms <- round((proc.time()[3] - module_t0) * 1000)
                                             if (module_ms >= 500) {
                                               self$log(sprintf("  module %s cleanup took %dms",
                                                                module_name, module_ms), 1)
                                             }
                                           }, error = function(e) {
                                             is_timeout <- grepl("time limit|Zeitlimit|reached elapsed",
                                                                 e$message,
                                                                 ignore.case = TRUE)
                                             if (isTRUE(is_timeout)) {
                                               self$log(paste("Error cleaning up", module_name, ": elapsed time limit reached"), 1)
                                             } else {
                                               self$log(paste("Error cleaning up", module_name, ":", e$message), 1)
                                             }
                                           })
                                         }
                                       },

                                       cleanup_system_resources = function(mode = c("session_end", "app_stop")) {
                                         mode <- match.arg(mode)
                                         self$log("Cleaning up system resources", 2)

                                         # 1) Kill and remove any background processes
                                         if (exists("shiny_bg_process", envir = globalenv())) {
                                           try(get("shiny_bg_process", envir = globalenv())$kill(), silent = TRUE)
                                           rm("shiny_bg_process", envir = globalenv())
                                           self$log("Background processes cleaned up", 2)
                                         }

                                         # 2) Optional connection cleanup. Disabled by default at
                                         # session end because closing some connection types can block.
                                         # Enable with options(miraprot.cleanup.close_connections = TRUE).
                                         if (isTRUE(getOption("miraprot.cleanup.close_connections", FALSE))) {
                                           private$fast_close_safe_connections(self$log)
                                         }

                                         # 3) Remove large session-scoped globals that can survive
                                         # browser restarts inside the same R process.
                                         private$cleanup_global_session_artifacts(self$log)

                                         # 4) Keep shutdown non-blocking. Skip synchronous gc() during
                                         # session end and app stop; object references are already removed.
                                         # Users can opt in if needed.
                                         if (isTRUE(getOption("miraprot.cleanup.run_gc", FALSE))) {
                                           gc(verbose = FALSE, full = FALSE)
                                           self$log("Garbage collection completed", 2)
                                         }
                                       }
                                     ),

                                     private = list(
                                       cleanup_functions = list(),
                                       debug_level = 1,
                                       cleanup_executed = FALSE,
                                       cleanup_global_session_artifacts = function(logger) {
                                         global_candidates <- c(
                                           "debug_gsea_wrapper",
                                           "debug_gsea_core",
                                           "debug_gsea_ranking_vector",
                                           "debug_gsea_gene_sets",
                                           "original_gsea_for_comparison"
                                         )

                                         existing <- global_candidates[vapply(
                                           global_candidates,
                                           exists,
                                           logical(1),
                                           envir = globalenv(),
                                           inherits = FALSE
                                         )]

                                         if (length(existing) > 0) {
                                           rm(list = existing, envir = globalenv())
                                           logger(paste("Removed global session artifacts:", paste(existing, collapse = ", ")), 1)
                                         }
                                       },
                                       fast_close_safe_connections = function(logger) {
                                         conns <- tryCatch(showConnections(all = TRUE), error = function(e) NULL)
                                         if (is.null(conns) || NROW(conns) == 0) return(invisible(NULL))

                                         if (!is.data.frame(conns)) {
                                           conns <- as.data.frame(conns, stringsAsFactors = FALSE)
                                         }

                                         # Close only local/file-like connections that are cheap and safe.
                                         # Skip sockets/servers/terminal streams which can block on close.
                                         safe_classes <- c("file", "gzfile", "bzfile", "xzfile", "textConnection")
                                         if (!("opened" %in% colnames(conns)) ||
                                             !("class" %in% colnames(conns)) ||
                                             !("description" %in% colnames(conns))) {
                                           return(invisible(NULL))
                                         }

                                         keep_mask <- conns$opened == "opened" &
                                           !is.na(conns$class) &
                                           conns$class %in% safe_classes &
                                           !grepl("stdin|stdout|stderr|server", conns$description, ignore.case = TRUE)

                                         target_ids <- suppressWarnings(as.integer(rownames(conns[keep_mask, , drop = FALSE])))
                                         target_ids <- target_ids[!is.na(target_ids)]
                                         if (length(target_ids) == 0) return(invisible(NULL))

                                         closed <- 0L
                                         for (conn_id in target_ids) {
                                           ok <- tryCatch({
                                             close(getConnection(conn_id))
                                             TRUE
                                           }, error = function(e) FALSE)
                                           if (isTRUE(ok)) closed <- closed + 1L
                                         }

                                         if (closed > 0) {
                                           logger(sprintf("Closed %d safe file/text connections", closed), 2)
                                         }
                                       }
                                     )
)

# Global instance (initialize with debug level)
cleanup_manager <- CentralCleanupManager$new(debug_level = 1)
