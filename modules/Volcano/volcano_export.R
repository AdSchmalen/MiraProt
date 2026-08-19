# ==============================================================================
# volcano_export.R
# ==============================================================================
#
# PURPOSE:
#   Export, download, clipboard, and grid integration helpers for the volcano
#   module. Provides a single copy_to_clipboard() implementation (eliminating
#   the previous three-site duplication), a download helper, a grid-add helper,
#   and a settings-reset helper.
#
# ARCHITECTURAL ROLE:
#   Export -- pure helper functions called by observers in the orchestrator
#   (and later in volcano_observers.R). Functions receive all necessary data
#   as arguments and perform side effects via session/runjs.
#
# RESPONSIBILITIES:
#   - Clipboard copy with scroll-preserving JavaScript fallback
#     (copy_to_clipboard)
#   - Plot download via ggsave (save_volcano_plot)
#   - Grid integration (add_volcano_to_grid)
#   - Axis/settings reset (reset_volcano_axis_settings)
#
# MUST NOT CONTAIN:
#   - Observer definitions (observeEvent, observe)
#   - Render functions (renderPlot, renderUI, etc.)
#   - Data transformation or plot generation logic
#   - Direct access to input$ (receives values as arguments)
#
# DEPENDENCIES:
#   Volcano sub-scripts:
#     - None (self-contained export utilities)
#   External packages:
#     - shinyjs: runjs (clipboard)
#     - ggplot2: ggsave (download)
#     - shiny: showNotification
#
# INTERACTIONS:
#   Called by:
#     - volcano_module.R (orchestrator): copyBtn, copy_selection, download,
#       add_to_grid, reset observers
#   Calls into:
#     - None
#   Data flow:
#     - IN:  text strings, ggplot objects, session references
#     - OUT: side effects (clipboard, file save, notifications)
#
# LAST UPDATED: 2026-03-10
# ==============================================================================

# ========================================
# Clipboard Copy (Single Implementation)
# ========================================

#' Copy text to the clipboard via JavaScript with scroll preservation.
#'
#' Uses Navigator.clipboard API where available, with a textarea-based
#' fallback for older browsers. Scroll position is saved and restored
#' to prevent the page from jumping.
#'
#' @param text       Character string to copy (newline-separated identifiers)
#' @param debug_log  Logging function
copy_to_clipboard <- function(text, debug_log = function(msg, level = 1) cat(msg, "\n")) {

  # Escape text for safe JavaScript string embedding
  escaped <- gsub("\\\\", "\\\\\\\\", text)
  escaped <- gsub('"', '\\\\"', escaped)
  escaped <- gsub("\n", "\\\\n", escaped)
  escaped <- gsub("\r", "\\\\r", escaped)

  runjs(paste0('
    (function() {
      var currentScrollTop = window.pageYOffset || document.documentElement.scrollTop;
      var currentScrollLeft = window.pageXOffset || document.documentElement.scrollLeft;

      function fallbackCopy(text) {
        var textArea = document.createElement("textarea");
        textArea.value = text;
        textArea.style.position = "fixed";
        textArea.style.left = "-999999px";
        textArea.style.top = "-999999px";
        textArea.style.width = "2em";
        textArea.style.height = "2em";
        textArea.style.padding = "0";
        textArea.style.border = "none";
        textArea.style.outline = "none";
        textArea.style.boxShadow = "none";
        textArea.style.background = "transparent";
        textArea.style.opacity = "0";
        textArea.style.pointerEvents = "none";
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        try {
          document.execCommand("copy");
        } catch (err) {
          console.error("Fallback clipboard copy failed:", err);
        }
        document.body.removeChild(textArea);
        window.scrollTo(currentScrollLeft, currentScrollTop);
      }

      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText("', escaped, '").then(function() {
          console.log("Clipboard copy successful");
        }, function(err) {
          console.error("navigator.clipboard failed, using fallback:", err);
          fallbackCopy("', escaped, '");
        });
      } else {
        fallbackCopy("', escaped, '");
      }
    })();
  '))

  debug_log("Clipboard copy executed via JavaScript", 2)
}

# ========================================
# Plot Download Helper
# ========================================

#' Save a ggplot object to file via ggsave.
#'
#' @param file   Destination file path (from downloadHandler)
#' @param plot   ggplot object to save
#' @param width  Width in inches
#' @param height Height in inches
#' @param dpi    Resolution in dots per inch
#' @param debug_log Logging function
#' @return TRUE on success, FALSE on failure
save_volcano_plot <- function(file, plot, width, height, dpi,
                              debug_log = function(msg, level = 1) cat(msg, "\n")) {
  tryCatch({
    ggsave(file, plot, width = width, height = height, dpi = dpi)
    debug_log(paste("Plot saved successfully:", file), 2)
    return(TRUE)
  }, error = function(e) {
    debug_log(paste("Error saving plot:", e$message), 1)
    showNotification(paste("Download error:", e$message), type = "error")
    return(FALSE)
  })
}

# ========================================
# Grid Integration Helper
# ========================================

#' Add a volcano plot to the central grid selection.
#'
#' Validates the plot, builds a sanitized plot ID from the optional label,
#' and calls the shared add_to_grid function.
#'
#' @param plot       ggplot object to add
#' @param label_raw  Raw label string from user input (may be NULL or empty)
#' @param ns         Shiny namespace function
#' @param rv         Reactive values (passed to add_to_grid)
#' @param modEnv     Module environment containing add_to_grid
#' @param debug_log  Logging function
#' @return TRUE on success, FALSE on failure
add_volcano_to_grid <- function(plot, label_raw, ns, rv, modEnv,
                                debug_log = function(msg, level = 1) cat(msg, "\n")) {

  if (is.null(plot)) {
    showNotification("No plot available to add.", type = "warning")
    debug_log("Grid add: plot is NULL", 1)
    return(FALSE)
  }

  if (!inherits(plot, "ggplot")) {
    showNotification("Only ggplot objects can be added to the grid.", type = "error")
    debug_log("Grid add: plot is not a ggplot", 1)
    return(FALSE)
  }

  sanitize <- function(x) gsub("[^[:alnum:]_]+", "_", x)
  lbl_id  <- if (is.null(label_raw) || !nzchar(label_raw)) "default" else sanitize(label_raw)
  plot_id <- paste0(ns(""), "Volcano_", lbl_id)
  lbl_vis <- if (!is.null(label_raw) && nzchar(label_raw)) label_raw else "Volcano"

  debug_log(paste("Adding to grid: id =", plot_id), 2)
  modEnv$add_to_grid(rv, id = plot_id, plot = plot, label = lbl_vis, source = "Volcano")
  showNotification("Added to grid selection.", type = "message")
  return(TRUE)
}

# ========================================
# Settings Reset Helper
# ========================================

#' Reset volcano internal axis state flags.
#'
#' @param volcano_state  The volcano reactive state object
#' @param debug_log      Logging function
reset_volcano_axis_settings <- function(volcano_state,
                                        debug_log = function(msg, level = 1) cat(msg, "\n")) {
  volcano_state$auto_range_set <- FALSE
  volcano_state$manual_axis_override <- FALSE
  volcano_state$auto_axis_update_in_progress <- FALSE
  debug_log("Volcano axis state flags reset", 2)
}
