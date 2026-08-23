# R/bootstrap.R
# ========================================
# Application Bootstrap: Library Loading, Options, Debug Management
# ========================================
# This file is auto-sourced by Shiny before app.R and all other R/ files.
# It sorts alphabetically first among R/ files, ensuring libraries and
# debug infrastructure are available to all subsequent code.

# ========================================
# Library Loading
# ========================================

.bootstrap_load_packages <- function(packages, source_label) {
  failed <- character()
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      failed <- c(failed, pkg)
    } else {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }
  }
  if (length(failed) > 0) {
    stop(sprintf("[%s] Missing packages: %s\nInstall with: install.packages(c(%s))",
                 source_label,
                 paste(failed, collapse = ", "),
                 paste(sprintf('"%s"', failed), collapse = ", ")),
         call. = FALSE)
  }
}

# --- Bioconductor packages ---
.bootstrap_load_packages(c(
  "AnnotationHub",
  "clusterProfiler",
  "enrichplot",
  "STRINGdb",
  "DEqMS",
  "BiocFileCache",
  "sva",            # ComBat Batch correction
  "GO.db",          # GO Children dropdown
  "AnnotationDbi",  # Annotation
  "biomaRt"        # Annotation
), source_label = "Bioconductor")

# --- CRAN packages ---
.bootstrap_load_packages(c(
  "shiny",          # Shiny apps
  "htmltools",      # HTML generation (core shiny dependency)
  "shinyjs",        # JavaScript operations in Shiny apps
  "magrittr",       # shiny required
  "shinydashboard", # Dashboard layout
  "rhandsontable",  # Interactive data tables (allows editing)
  "plotly",         # Interactive plots
  "DT",             # Interactive data tables
  "readxl",         # Load excel files and sheets
  "writexl",        # Excel writing
  "reshape2",       # Data manipulation
  "ggplot2",        # Visualize plots
  "ggrepel",        # Label dots in ggplot dotplot
  "viridis",        # Color Palette
  "stringr",        # string functions
  "gridExtra",      # Arrange plots in a grid (Volcano plots)
  "doParallel",     # CPU parallelization for demanding calculations
  "parallel",       # CPU parallelization
  "foreach",        # Complements doParallel
  "cowplot",        # Arrange plots in a grid (i.e. Pubmed plots of GSEA results)
  "igraph",         # Static String network
  "visNetwork",     # Interactive STRING network
  "dplyr",          # Data manipulation
  "RColorBrewer",   # Color palette for STRING network
  "ComplexUpset",   # Upset plots
  "VennDiagram",    # Venn Diagram
  "circlize",       # Color palette for heatmap
  "missForest",     # Random Forest Imputation
  "mice",           # MICE imputation
  "stats",          # Statistical testing
  "pracma",         # Estimation of Hyper-parameter (Limma/DEqMS)
  "colourpicker",   # UI colourpicker
  "scales",         # Breaks and labels for axes and legends
  "shinyalert",     # Warnings for shiny applications
  "svglite",        # SVG graphics driver
  "tidyverse",      # Package collection for data analysis and visualization
  "purrr",          # data transformation list -> dataframe
  "openxlsx",       # Create XLSX workbooks
  "ggupset",        # Upset Gene Ontology
  "ggridges",       # Ridgeline plot GSEA
  "grid",           # Extract column name from plot
  "umap",           # UMAP calculation (PCA tab)
  "rlang",
  "sortable",       # Drag & drop functionality for plot reordering
  "png",            # Heatmap to PNG to ggplot
  "rsvg",           # Heatmap to SVG to ggplot
  "xml2",           # Heatmap to SVG to ggplot
  "bslib",
  "curl",
  "later"           # Debouncing (Plot Grid)
), source_label = "CRAN")

# --- Additional runtime packages ---
.bootstrap_load_packages(c(
  "shinyTree"
), source_label = "CRAN/GitHub")

# Clean up helper (not needed after loading)
rm(.bootstrap_load_packages)

# ========================================
# Global Options
# ========================================

options(datawizard.debug = FALSE)   # TRUE for detailed Debug info
options(datawizard.verbose = TRUE)  # FALSE for Silent mode

options(shiny.maxRequestSize = 500 * 1024^2) # allow file uploads up to 500MB

options(shiny.error = function() {
  message("An error has occurred: ", geterrmessage())
})

# ========================================
# Debug Management
# ========================================

# Canonical debug level. Pinned to globalenv() because Shiny's loadSupport()
# sources R/ into a dedicated support environment, not .GlobalEnv. Pinning
# here guarantees that readers using get0(..., envir = globalenv()) and
# writers using assign(..., envir = globalenv()) share a single binding.
if (is.null(get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE))) {
  assign("DEBUG_LEVEL", 0L, envir = globalenv())  # 0 = Essential (default)
}

# Structured log buffer.  Every debug_log() call appends ONE row regardless
# of level.  Console gating happens per-row at capture time (inside
# .miraprot_log_record); display gating happens at render time.
# Cap is FIFO: when size > .miraprot_log_max, oldest entries are dropped.
#
# IMPORTANT: We pin everything to globalenv() explicitly. Shiny's
# loadSupport() auto-sources files in R/ into a dedicated "support"
# environment that is NOT .GlobalEnv.  If we relied on <- / <<- here,
# the buffer would live in the support env while readers using
# get0(..., envir = globalenv(), inherits = FALSE) would never see it.
#
# NOTE: The buffer and version counter are always reset (no is.null guard)
# so that every new app start / hot-reload begins with a fresh log.
# DEBUG_LEVEL is the only variable kept across reloads (see above).
assign(
  ".miraprot_log_buffers",
  list(`0` = data.frame(
    time    = as.POSIXct(character(0)),
    level   = integer(0),
    tag     = character(0),
    message = character(0),
    line    = character(0), run_id = character(0), event_id = character(0),
    stringsAsFactors = FALSE
  ), `1` = data.frame(
    time    = as.POSIXct(character(0)),
    level   = integer(0),
    tag     = character(0),
    message = character(0),
    line    = character(0), run_id = character(0), event_id = character(0),
    stringsAsFactors = FALSE
  ), `2` = data.frame(
    time    = as.POSIXct(character(0)),
    level   = integer(0),
    tag     = character(0),
    message = character(0),
    line    = character(0), run_id = character(0), event_id = character(0),
    stringsAsFactors = FALSE
  )),
  envir = globalenv()
)
if (is.null(get0(".miraprot_log_max", envir = globalenv(), inherits = FALSE))) {
  assign(".miraprot_log_max", 5000L, envir = globalenv())
}
# Monotonically increasing version counter — cheap invalidation token for
# the Session-tab reactivePoll so it doesn't diff the whole data frame.
# Always reset alongside the buffer so the counter stays consistent.
assign(".miraprot_log_version", 0L, envir = globalenv())

# Join stored log records for UI/export paths.  Records are kept as one
# character entry per captured event; rendering/exporting is the only place
# that inserts separators.  Trimming record-edge CR/LF characters prevents
# accidental blank lines if a legacy source already supplied newline-terminated
# text.
assign(".miraprot_log_records_to_text", function(records) {
  if (is.null(records)) return("")
  records <- as.character(records)
  records <- records[!is.na(records)]
  if (length(records) == 0L) return("")
  records <- gsub("^[\r\n]+|[\r\n]+$", "", records, perl = TRUE)
  records <- records[nzchar(records)]
  if (length(records) == 0L) return("")
  paste0(paste(records, collapse = "\n"), "\n")
}, envir = globalenv())

# Shared record helper.  Signature: (level, tag, message).
# (a) Appends every entry unconditionally — level metadata preserved.
# (b) Gates cat() by the live DEBUG_LEVEL at *capture* time only.
#     Changing the level later never replays old entries to stdout.
# (c) Bumps .miraprot_log_version for cheap reactive invalidation.
assign(".miraprot_log_record", function(level, tag, message, run_id = "", event_id = "") {
  tryCatch({
    ge        <- globalenv()
    timestamp <- Sys.time()
    message   <- paste(as.character(message), collapse = " ")
    message   <- gsub("[\r\n]+", " ", message, perl = TRUE)
    message   <- trimws(message)
    line      <- paste0("[ ", tag, " ", format(timestamp, "%H:%M:%S"), " ] ", message)

    # (b) console gate — read DEBUG_LEVEL at capture time
    live_level <- get0("DEBUG_LEVEL", envir = ge, inherits = FALSE)
    if (is.numeric(live_level) && length(live_level) == 1L && live_level >= level) {
      cat(line, "\n")
    }

    # (a) always store
    buffers <- get0(".miraprot_log_buffers", envir = ge, inherits = FALSE)
    max_n <- get0(".miraprot_log_max",    envir = ge, inherits = FALSE)
    if (is.null(max_n) || !is.numeric(max_n) || length(max_n) != 1L) max_n <- 5000L
    row <- data.frame(
      time    = timestamp,
      level   = as.integer(level),
      tag     = as.character(tag),
      message = as.character(message),
      line    = line,
      run_id  = as.character(run_id),
      event_id = as.character(event_id),
      stringsAsFactors = FALSE
    )
    lvl_int <- suppressWarnings(as.integer(level))
    if (is.na(lvl_int) || !(lvl_int %in% 0:2)) lvl_int <- 2L
    if (is.null(buffers) || !is.list(buffers)) buffers <- list()
    target_keys <- as.character(seq.int(from = lvl_int, to = 2L))
    for (lvl_key in target_keys) {
      lvl_buf <- buffers[[lvl_key]]
      new_buf <- if (is.null(lvl_buf) || !is.data.frame(lvl_buf)) row else rbind(lvl_buf, row)
      if (nrow(new_buf) > max_n) {
        new_buf <- new_buf[(nrow(new_buf) - max_n + 1L):nrow(new_buf), , drop = FALSE]
      }
      buffers[[lvl_key]] <- new_buf
    }
    assign(".miraprot_log_buffers", buffers, envir = ge)

    # (c) version bump
    v <- get0(".miraprot_log_version", envir = ge, inherits = FALSE)
    assign(".miraprot_log_version", if (is.integer(v)) v + 1L else 1L, envir = ge)
  }, error = function(e) invisible(NULL))
}, envir = globalenv())

# Main app debug_log — forwards to the centralised record function.
# Falls back to a direct cat() when .miraprot_log_record is not yet
# initialised (e.g. unit tests, standalone sourcing).
debug_log <- function(message, level = 1) {
  rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
  if (is.function(rec)) {
    rec(level, "MAIN APP", message)
  } else {
    effective_level <- tryCatch(
      get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
      error = function(e) 0L
    )
    if (is.numeric(effective_level) && effective_level >= level) {
      cat(paste0("[ MAIN APP ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }
}

MIRAPROT_IN_PORTABLE <- nzchar(Sys.getenv("MIRAPROT_IN_PORTABLE", ""))

if (MIRAPROT_IN_PORTABLE) {
  debug_log("Running in portable desktop mode", 1)
  debug_log(paste("Port:", Sys.getenv("MIRAPROT_PORT", "3838")), 1)

  # Ensure AnnotationHub uses the launcher-provided persistent cache
  .portable_ah_cache <- Sys.getenv("ANNOTATION_HUB_CACHE", "")
  if (nzchar(.portable_ah_cache)) {
    if (!dir.exists(.portable_ah_cache)) dir.create(.portable_ah_cache, recursive = TRUE)
    debug_log(paste("AnnotationHub cache:", .portable_ah_cache), 1)
  }
  # Store for restoration after force-refresh operations
  options(miraprot.annotation_hub_cache = .portable_ah_cache)
  rm(.portable_ah_cache)

  # Portable mode always stops on browser close (single-user desktop)
  options(miraprot.stop_on_close = TRUE)

  # Log key portable paths
  debug_log(paste("GO cache:", Sys.getenv("MIRAPROT_GO_CACHE", "")), 1)
  debug_log(paste("Log dir:", Sys.getenv("MIRAPROT_LOG_DIR", "")), 1)
  debug_log(paste("R libs:", Sys.getenv("R_LIBS_USER", "")), 1)
}

debug_log(sprintf("Environment: R %s, %s, %s, cores=%s%s",
                  paste(R.version$major, R.version$minor, sep = "."),
                  .Platform$OS.type,
                  Sys.info()[["sysname"]],
                  tryCatch(as.character(parallel::detectCores()), error = function(e) "?"),
                  if (MIRAPROT_IN_PORTABLE) ", portable=TRUE" else ""), 2)
