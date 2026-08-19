# R/health_endpoint.R
# ========================================
# Health Endpoint for Go Launcher Idle Monitor
# ========================================
# Provides a lightweight /__health HTTP endpoint that reports
# the number of active Shiny sessions. The Go launcher's idle
# monitor queries this to decide when to auto-shutdown.
#
# Only active when MIRAPROT_IN_PORTABLE is TRUE.
# Depends on: MIRAPROT_IN_PORTABLE from R/bootstrap.R

# Session counter (shared across all sessions via a private environment)
.health_env <- new.env(parent = emptyenv())
.health_env$session_count <- 0L

health_on_session_start <- function() {
  .health_env$session_count <- .health_env$session_count + 1L
}

health_on_session_end <- function() {
  .health_env$session_count <- max(0L, .health_env$session_count - 1L)
}

# Register the /__health route via Shiny's HTTP response filter.
# The filter intercepts responses before they reach the client;
# for /__health requests we replace the response with our JSON payload.
if (exists("MIRAPROT_IN_PORTABLE") && isTRUE(MIRAPROT_IN_PORTABLE)) {
  options(shiny.http.response.filter = function(req, response) {
    if (identical(req$PATH_INFO, "/__health")) {
      body <- sprintf('{"status":"ok","sessions":%d}', .health_env$session_count)
      return(shiny::httpResponse(
        status = 200L,
        content_type = "application/json",
        content = body
      ))
    }
    response
  })
}
