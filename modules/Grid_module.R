# modules/Grid_module.R
#
# Purpose:
#   Orchestrator for the Grid Composer module.  Loads the utility layers and
#   exposes modGridUI() and modGridServer() to the application.
#
# Architecture:
#   This file is the sole server entry point for the Grid module.  All
#   application logic for composing, rendering, and downloading the plot grid
#   lives in the server function below.  Supporting utilities are split across
#   three focused files in modules/Grid/:
#
#   - Grid_layout.R      Selection management, labels, span/margin state,
#                         layout optimisation algorithms.
#   - Grid_legend.R      Legend detection and legend-position control.
#   - Grid_composition.R Plot preparation, alignment, span placement,
#                         grid assembly via cowplot.
#   - Grid_ui.R          Pure UI definition (no logic).
#
# Structure:
#   1. Source sub-files into modEnv.
#   2. modGridUI() - thin wrapper around grid_UI().
#   3. modGridServer() - the only server function for this module:
#      a. State initialisation
#      b. grid_settings reactive
#      c. selected_plots reactive
#      d. Selection UI render + per-plot input observers (registry pattern)
#      e. Grid management observers (clear, add blank, optimise, remove)
#      f. Composition helpers (compose_from, render_data debounce)
#      g. Render and download handlers
#      h. Session cleanup
#
# Future developers:
#   - Do not add server logic to any other Grid file.
#   - debug_log is defined in the server closure and passed explicitly to
#     utility functions that need it.
#   - rv is passed explicitly to composition functions that require source
#     metadata for alignment or legend exclusions.
#   - The registry pattern (local + registered vector) is used for dynamic
#     observers to prevent accumulation when the plot list grows.

sys.source("./modules/Grid/Grid_ui.R",          envir = modEnv)
sys.source("./modules/Grid/Grid_layout.R",      envir = modEnv)
sys.source("./modules/Grid/Grid_legend.R",      envir = modEnv)
sys.source("./modules/Grid/Grid_composition.R", envir = modEnv)

modGridUI <- function(id) {
  ns <- NS(id)
  tagList(
    modEnv$grid_UI(ns)
  )
}

modGridServer <- function(id, rv, module_outputs = NULL, debug_level = 0) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "GRID MODULE", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ GRID MODULE ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("GRID module server starting", 1)

    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    modEnv$`%||%` <- `%||%`

    # Warn once if cowplot is absent.
    observe({
      if (!requireNamespace("cowplot", quietly = TRUE)) {
        showNotification(
          "Package 'cowplot' is required for the grid composer.",
          type = "error", duration = 8
        )
        debug_log("cowplot not available - preview and download will fail", 1)
      }
    })

    # -------------------------------------------------------------------------
    # a. State initialisation
    # -------------------------------------------------------------------------
    if (is.null(isolate(rv$gridplot_selection))) {
      rv$gridplot_selection <- list()
      debug_log("Initialised rv$gridplot_selection", 2)
    }
    if (is.null(isolate(rv$gridplot_order))) {
      rv$gridplot_order <- character(0)
      debug_log("Initialised rv$gridplot_order", 2)
    }
    if (is.null(isolate(rv$plot_margins))) {
      rv$plot_margins <- list()
      debug_log("Initialised rv$plot_margins", 2)
    }

    # -------------------------------------------------------------------------
    # b. grid_settings reactive
    # -------------------------------------------------------------------------
    grid_settings <- reactive({
      list(
        nrow = { v <- input$nrow; if (is.null(v)) 1L else max(1L, as.integer(v)) },
        ncol = { v <- input$ncol; if (is.null(v)) 2L else max(1L, as.integer(v)) },
        align = { v <- input$align; if (is.null(v) || !nzchar(v)) "none" else v },
        labels_mode = { v <- input$labels_mode;
                         if (is.null(v) || !nzchar(v)) "none" else v },
        labels_custom = { input$labels_custom %||% "" },
        label_size = { v <- input$label_size;
                        if (is.null(v)) 12 else max(6, min(72, as.numeric(v))) },
        hide_titles = { isTRUE(input$hide_titles) },
        margins = list(
          top    = { v <- input$margin_top;
                      if (is.null(v)) 10 else max(0, min(100, as.numeric(v))) },
          right  = { v <- input$margin_right;
                      if (is.null(v)) 10 else max(0, min(100, as.numeric(v))) },
          bottom = { v <- input$margin_bottom;
                      if (is.null(v)) 10 else max(0, min(100, as.numeric(v))) },
          left   = { v <- input$margin_left;
                      if (is.null(v)) 10 else max(0, min(100, as.numeric(v))) }
        ),
        plot_spans   = rv$plot_spans   %||% list(),
        plot_margins = rv$plot_margins %||% list(),
        force_legend_position = { v <- input$force_legend_position;
                                    if (is.null(v) || !nzchar(v)) "preserve" else v }
      )
    })

    # Import utilities from modEnv.
    add_to_grid                 <- modEnv$add_to_grid
    add_blank_to_grid           <- modEnv$add_blank_to_grid
    generate_blank_id           <- modEnv$generate_blank_id
    remove_from_grid            <- modEnv$remove_from_grid
    clear_grid                  <- modEnv$clear_grid
    build_labels                <- modEnv$build_labels
    compose_grid                <- modEnv$compose_grid
    prepare_plots_for_grid      <- modEnv$prepare_plots_for_grid
    apply_per_plot_margins      <- modEnv$apply_per_plot_margins
    get_include_map             <- modEnv$get_include_map
    ensure_span_entry           <- modEnv$ensure_span_entry
    clamp_spans_to_grid         <- modEnv$clamp_spans_to_grid
    compute_optimal_grid_layout <- modEnv$compute_optimal_grid_layout
    .move_swap                  <- modEnv$.move_swap
    can_pack_spans_greedy       <- modEnv$can_pack_spans_greedy
    min_rows_for_cols           <- modEnv$min_rows_for_cols
    reorder_compact_fit         <- modEnv$reorder_compact_fit

    # -------------------------------------------------------------------------
    # c. selected_plots reactive
    # -------------------------------------------------------------------------
    selected_plots <- reactive({
      ids <- rv$gridplot_order
      if (length(ids) == 0) return(list())
      plots <- list()
      for (id in ids) {
        entry <- rv$gridplot_selection[[id]]
        if (!is.null(entry) && !is.null(entry$plot)) plots[[id]] <- entry$plot
      }
      plots
    })

    # Resolve the lightweight references restored by session management only
    # after plot-producing modules have had an opportunity to rebuild.  Module
    # output APIs are intentionally heterogeneous, so this adapter considers
    # only explicitly plot-named, zero-argument output functions and never
    # walks arbitrary module state.
    resolve_grid_plot <- function(ref) {
      if (!is.list(ref) || is.null(module_outputs)) return(NULL)
      outputs <- tryCatch(isolate(reactiveValuesToList(module_outputs)),
                          error = function(e) module_outputs)
      if (!is.list(outputs)) return(NULL)
      norm <- function(x) tolower(gsub("[^[:alnum:]]", "", x %||% ""))
      wanted_module <- norm(ref$source_module)
      module_names <- names(outputs) %||% character()
      module_idx <- which(vapply(module_names, function(x) {
        key <- sub("_out$", "", x, ignore.case = TRUE)
        identical(norm(key), wanted_module) ||
          startsWith(norm(key), wanted_module) || startsWith(wanted_module, norm(key))
      }, logical(1L)))
      if (!length(module_idx)) return(NULL)
      api <- outputs[[module_idx[[1L]]]]
      if (!is.list(api)) return(NULL)
      candidate_names <- grep("plot", names(api) %||% character(),
                              value = TRUE, ignore.case = TRUE)
      candidates <- lapply(candidate_names, function(nm) {
        fn <- api[[nm]]
        if (!is.function(fn) || length(formals(fn)) != 0L) return(NULL)
        tryCatch(isolate(fn()), error = function(e) NULL)
      })
      candidates <- Filter(function(x) inherits(x, "ggplot"), candidates)
      if (!length(candidates)) return(NULL)
      wanted <- as.character(ref$source_plot_id %||% "")[1L]
      title_match <- vapply(candidates, function(p) {
        title <- tryCatch(as.character(p$labels$title %||% "")[1L],
                          error = function(e) "")
        nzchar(wanted) && identical(title, wanted)
      }, logical(1L))
      if (any(title_match)) candidates[[which(title_match)[[1L]]]] else
        if (length(candidates) == 1L) candidates[[1L]] else NULL
    }

    observeEvent(rv$session_restore_trigger, {
      pending <- isolate(rv$grid_session_payload)
      if (!is.list(pending) || !length(pending)) return()
      attempt <- 0L
      resolved <- list()
      # Retries run through later::later(), outside the observer that armed
      # them. Keep every shared reactive-value access in an explicit reactive
      # context so an unresolved Grid payload cannot terminate the app while
      # unrelated modules are being restored.
      restore_pending <- function() isolate({
        attempt <<- attempt + 1L
        restored <- list()
        unresolved <- list()
        for (entry in pending) {
          plot <- resolve_grid_plot(entry)
          if (is.null(plot)) {
            unresolved[[length(unresolved) + 1L]] <- entry
            next
          }
          id <- as.character(entry$stable_plot_id)[1L]
          restored[[id]] <- list(
            plot = plot,
            label = entry$display_label,
            source = entry$source_module,
            source_plot_id = entry$source_plot_id,
            type = "ggplot",
            include_label = isTRUE(entry$include_label)
          )
        }
        resolved <<- c(resolved, restored)
        if (length(unresolved) && attempt < 20L) {
          pending <<- unresolved
          later::later(restore_pending, delay = 0.1)
          return()
        }
        # Never retain a serialized plot fallback from the uploaded payload.
        rv$gridplot_selection <- resolved
        all_saved <- isolate(rv$grid_session_payload)
        rv$plot_spans <- stats::setNames(lapply(all_saved, `[[`, "span"),
                                         vapply(all_saved, `[[`, character(1L), "stable_plot_id"))
        rv$plot_margins <- stats::setNames(lapply(all_saved, `[[`, "margins"),
                                           vapply(all_saved, `[[`, character(1L), "stable_plot_id"))
        resolved_ids <- names(rv$gridplot_selection) %||% character()
        saved_order <- vapply(isolate(rv$grid_session_payload),
                              function(x) as.character(x$stable_plot_id)[1L], character(1L))
        rv$gridplot_order <- saved_order[saved_order %in% resolved_ids]
        if (length(unresolved)) for (entry in unresolved) {
          warning(sprintf("Grid restore skipped unresolved plot '%s' from module '%s'.",
                          entry$source_plot_id, entry$source_module), call. = FALSE)
        }
        rv$grid_session_payload <- NULL
      })
      session$onFlushed(restore_pending, once = TRUE)
    }, ignoreInit = TRUE)

    # -------------------------------------------------------------------------
    # d. Selection UI render
    # -------------------------------------------------------------------------
    output$selection <- renderUI({
      sel <- rv$gridplot_selection %||% list()
      ord <- rv$gridplot_order     %||% character(0)

      debug_log(paste("Selection UI render - count:", length(ord)), 2)

      if (!length(ord)) return(div(em("No plots selected yet.")))

      max_cols <- if (is.null(input$ncol)) 4L else as.integer(input$ncol)
      max_rows <- if (is.null(input$nrow)) 4L else as.integer(input$nrow)
      if (!is.finite(max_cols) || max_cols < 1) max_cols <- 4L
      if (!is.finite(max_rows) || max_rows < 1) max_rows <- 4L

      tagList(lapply(seq_along(ord), function(i) {
        id      <- ord[[i]]
        entry   <- sel[[id]]
        key_id  <- gsub("[^[:alnum:]_]+", "_", id)
        include_val <- isTRUE(entry$include_label %||% TRUE)

        sp      <- rv$plot_spans[[id]] %||% list(colspan = 1L, rowspan = 1L)
        cur_col <- as.integer(if (is.null(sp$colspan)) 1L else sp$colspan)
        cur_row <- as.integer(if (is.null(sp$rowspan)) 1L else sp$rowspan)
        if (!is.finite(cur_col) || cur_col < 1) cur_col <- 1L
        if (!is.finite(cur_row) || cur_row < 1) cur_row <- 1L

        pm            <- rv$plot_margins[[id]] %||%
          list(top = 0, right = 0, bottom = 0, left = 0)
        cur_pm_top    <- as.numeric(pm$top    %||% 0)
        cur_pm_right  <- as.numeric(pm$right  %||% 0)
        cur_pm_bottom <- as.numeric(pm$bottom %||% 0)
        cur_pm_left   <- as.numeric(pm$left   %||% 0)

        wellPanel(
          h5(id),
          fluidRow(
            column(
              4,
              checkboxInput(ns(paste0("include_label_", key_id)),
                            "Include in labeling", value = include_val),
              div(style = "margin-top: 8px;",
                  actionButton(ns(paste0("move_up_",   key_id)), "Up",
                               class = "btn btn-sm btn-outline-secondary"),
                  HTML("&nbsp;"),
                  actionButton(ns(paste0("move_down_", key_id)), "Down",
                               class = "btn btn-sm btn-outline-secondary")
              )
            ),
            column(
              8,
              fluidRow(
                column(6,
                       numericInput(ns(paste0("colspan_", key_id)), "Cols",
                                    value = min(cur_col, max_cols),
                                    min = 1, max = max_cols, step = 1,
                                    width = "100%")),
                column(6,
                       numericInput(ns(paste0("rowspan_", key_id)), "Rows",
                                    value = min(cur_row, max_rows),
                                    min = 1, max = max_rows, step = 1,
                                    width = "100%"))
              )
            )
          ),
          div(
            style = "margin-top: 6px;",
            tags$small("Margin offset (pt):", style = "color: #555;"),
            fluidRow(
              column(3, numericInput(ns(paste0("pm_top_",    key_id)),
                                     "Top",    value = cur_pm_top,
                                     min = -200, max = 500, step = 5,
                                     width = "100%")),
              column(3, numericInput(ns(paste0("pm_right_",  key_id)),
                                     "Right",  value = cur_pm_right,
                                     min = -200, max = 500, step = 5,
                                     width = "100%")),
              column(3, numericInput(ns(paste0("pm_bottom_", key_id)),
                                     "Bottom", value = cur_pm_bottom,
                                     min = -200, max = 500, step = 5,
                                     width = "100%")),
              column(3, numericInput(ns(paste0("pm_left_",   key_id)),
                                     "Left",   value = cur_pm_left,
                                     min = -200, max = 500, step = 5,
                                     width = "100%"))
            )
          ),
          div(style = "margin-top: 8px;",
              actionButton(ns(paste0("remove_", key_id)), "Remove",
                           class = "btn btn-sm btn-outline-danger")
          )
        )
      }))
    })

    # Registry: register include-label observer once per plot ID.
    local({
      registered <- character(0)
      observe({
        ord <- rv$gridplot_order %||% character(0)
        if (!length(ord)) return()
        new_ids <- setdiff(ord, registered)
        if (!length(new_ids)) return()
        lapply(new_ids, function(id) {
          key_id   <- gsub("[^[:alnum:]_]+", "_", id)
          input_id <- paste0("include_label_", key_id)
          observeEvent(input[[input_id]], ignoreInit = TRUE, {
            val <- isTRUE(input[[input_id]])
            if (!is.null(rv$gridplot_selection[[id]]))
              rv$gridplot_selection[[id]]$include_label <- val
          })
        })
        registered <<- c(registered, new_ids)
      })
    })

    # Registry: register Up / Down move observers once per plot ID.
    local({
      registered <- character(0)
      observe({
        ord <- rv$gridplot_order %||% character(0)
        if (!length(ord)) return()
        new_ids <- setdiff(ord, registered)
        if (!length(new_ids)) return()
        lapply(new_ids, function(id) {
          key_id <- gsub("[^[:alnum:]_]+", "_", id)
          observeEvent(input[[paste0("move_up_", key_id)]], ignoreInit = TRUE, {
            current <- rv$gridplot_order %||% character(0)
            pos <- match(id, current, nomatch = 0L)
            if (pos > 1L) rv$gridplot_order <- .move_swap(current, pos, "up")
          })
          observeEvent(input[[paste0("move_down_", key_id)]], ignoreInit = TRUE, {
            current <- rv$gridplot_order %||% character(0)
            pos <- match(id, current, nomatch = 0L)
            if (pos > 0L && pos < length(current))
              rv$gridplot_order <- .move_swap(current, pos, "down")
          })
        })
        registered <<- c(registered, new_ids)
      })
    })

    # Registry: register colspan, rowspan, and per-plot margin observers
    # once per plot ID.
    local({
      registered <- character(0)
      observe({
        ord <- rv$gridplot_order %||% character(0)
        if (!length(ord)) return()
        new_ids <- setdiff(ord, registered)
        if (!length(new_ids)) return()
        lapply(new_ids, function(id) {
          key_id <- gsub("[^[:alnum:]_]+", "_", id)

          observeEvent(input[[paste0("colspan_", key_id)]], ignoreInit = TRUE, {
            ensure_span_entry(rv, id)
            val <- max(1L,
                       min(as.integer(input$ncol %||% 4L),
                           as.integer(input[[paste0("colspan_", key_id)]] %||% 1L)))
            rv$plot_spans[[id]]$colspan <- val
          })
          observeEvent(input[[paste0("rowspan_", key_id)]], ignoreInit = TRUE, {
            ensure_span_entry(rv, id)
            val <- max(1L,
                       min(as.integer(input$nrow %||% 4L),
                           as.integer(input[[paste0("rowspan_", key_id)]] %||% 1L)))
            rv$plot_spans[[id]]$rowspan <- val
          })

          register_pm_observer <- function(side, local_id, local_key_id) {
            force(side); force(local_id); force(local_key_id)
            observeEvent(input[[paste0("pm_", side, "_", local_key_id)]],
                         ignoreInit = TRUE, {
              raw <- input[[paste0("pm_", side, "_", local_key_id)]]
              val <- suppressWarnings(as.numeric(raw))
              if (!is.null(val) && length(val) == 1L && is.finite(val)) {
                val <- max(-200, min(500, val))
                if (is.null(rv$plot_margins[[local_id]]))
                  rv$plot_margins[[local_id]] <-
                    list(top = 0, right = 0, bottom = 0, left = 0)
                rv$plot_margins[[local_id]][[side]] <- val
              }
            })
          }
          for (side in c("top", "right", "bottom", "left"))
            register_pm_observer(side, id, key_id)
        })
        registered <<- c(registered, new_ids)
      })
    })

    observe({
      nr <- input$nrow
      nc <- input$ncol
      req(is.numeric(nr), is.numeric(nc), nr >= 1, nc >= 1)
      clamp_spans_to_grid(rv, as.integer(nr), as.integer(nc),
                           debug_log = debug_log)
    })

    # -------------------------------------------------------------------------
    # e. Grid management observers
    # -------------------------------------------------------------------------

    observeEvent(input$add_blank, {
      tryCatch({
        id <- generate_blank_id(rv, debug_log = debug_log)
        add_blank_to_grid(rv, id = id, debug_log = debug_log)
      }, error = function(e) {
        debug_log(paste("Add blank failed:", e$message), 1)
      })
    })

    observeEvent(input$clear, {
      debug_log("Clear selection requested", 1)
      clear_grid(rv)
    })

    observeEvent(input$optimize_grid, {
      plots <- selected_plots()
      n     <- length(plots)
      if (n <= 0) {
        showNotification("No plots to optimise.", type = "message")
        debug_log("Optimise clicked with no plots", 1)
        return()
      }

      settings  <- grid_settings()
      spans     <- rv$plot_spans %||% list()
      have_spans <- length(spans) > 0 && any(vapply(spans, function(sp) {
        (as.integer(sp$colspan %||% 1L)) > 1L ||
          (as.integer(sp$rowspan %||% 1L)) > 1L
      }, logical(1)))

      widthIn  <- { x <- input$plotWidthInch;
                     if (is.null(x)) 14 else suppressWarnings(as.numeric(x)) }
      heightIn <- { x <- input$plotHeightInch;
                     if (is.null(x)) 10 else suppressWarnings(as.numeric(x)) }
      if (!is.finite(widthIn)  || widthIn  <= 0) widthIn  <- 14
      if (!is.finite(heightIn) || heightIn <= 0) heightIn <- 10
      container_ratio <- widthIn / heightIn

      cur_r         <- as.integer(settings$nrow)
      cur_c         <- as.integer(settings$ncol)
      current_order <- rv$gridplot_order %||% character(0)
      improved  <- FALSE
      new_order <- current_order
      new_r     <- cur_r
      new_c     <- cur_c

      if (have_spans) {
        baseline_rows <- min_rows_for_cols(spans, ncol = cur_c,
                                            order_ids = current_order,
                                            max_rows = max(50L, cur_r + 20L))
        compact_candidate <- reorder_compact_fit(spans, current_order,
                                                  ncol = cur_c)
        compact_rows <- min_rows_for_cols(spans, ncol = cur_c,
                                           order_ids = compact_candidate,
                                           max_rows = max(50L, cur_r + 20L))

        debug_log(
          paste("Optimise: baseline rows =", baseline_rows,
                "| compact rows =", compact_rows, "at", cur_c, "cols"),
          1
        )

        if (is.finite(compact_rows) && compact_rows < baseline_rows) {
          if (compact_rows < cur_r) new_r <- compact_rows
          new_order <- compact_candidate
          improved  <- TRUE
        }

        opt_current <- tryCatch(
          compute_optimal_grid_layout(
            n = n, container_ratio = container_ratio,
            max_rows = max(cur_r, 10L), max_cols = max(cur_c, 10L),
            strategy = "balanced", prefer_fewer_empty = TRUE,
            spans = spans, order_ids = current_order,
            enforce_pack = TRUE, debug_log = debug_log
          ),
          error = function(e) NULL
        )

        opt_compact <- NULL
        if (isTRUE(input$opt_compact_order)) {
          opt_compact <- tryCatch(
            compute_optimal_grid_layout(
              n = n, container_ratio = container_ratio,
              max_rows = max(cur_r, 10L), max_cols = max(cur_c, 10L),
              strategy = "balanced", prefer_fewer_empty = TRUE,
              spans = spans, order_ids = compact_candidate,
              enforce_pack = TRUE, debug_log = debug_log
            ),
            error = function(e) NULL
          )
        }

        pick_opt <- function(opt) {
          if (is.null(opt) || !isTRUE(opt$pack_ok)) return(NULL)
          if ((opt$nrow * opt$ncol) < (cur_r * cur_c)) return(opt)
          if ((opt$nrow <= cur_r && opt$ncol <= cur_c) &&
              (opt$nrow < cur_r  || opt$ncol < cur_c)) return(opt)
          NULL
        }
        chosen <- pick_opt(opt_compact)
        if (is.null(chosen)) chosen <- pick_opt(opt_current)

        if (!is.null(chosen)) {
          new_r <- min(new_r, chosen$nrow)
          new_c <- min(new_c, chosen$ncol)
          if (!is.null(opt_compact) && identical(chosen, opt_compact))
            new_order <- compact_candidate
          improved <- TRUE
        }
      } else {
        opt <- tryCatch(
          compute_optimal_grid_layout(
            n = n, container_ratio = container_ratio,
            max_rows = max(cur_r, 10L), max_cols = max(cur_c, 10L),
            strategy = "balanced", prefer_fewer_empty = TRUE,
            spans = NULL, order_ids = NULL, enforce_pack = FALSE,
            debug_log = debug_log
          ),
          error = function(e) NULL
        )
        if (!is.null(opt)) {
          if ((opt$nrow * opt$ncol) < (cur_r * cur_c) ||
              ((opt$nrow <= cur_r && opt$ncol <= cur_c) &&
               (opt$nrow < cur_r || opt$ncol < cur_c))) {
            new_r <- opt$nrow; new_c <- opt$ncol; improved <- TRUE
          }
        }
      }

      if (improved) {
        if (!identical(new_order, current_order) &&
            isTRUE(input$opt_compact_order)) {
          rv$gridplot_order <- new_order
          debug_log(
            paste("Optimise: compact order ->",
                  paste(new_order, collapse = ", ")),
            1
          )
        }
        if (new_r != cur_r) updateNumericInput(session, "nrow", value = new_r)
        if (new_c != cur_c) updateNumericInput(session, "ncol", value = new_c)
        clamp_spans_to_grid(rv, new_r, new_c, debug_log = debug_log)

        msg <- sprintf(
          "Optimised layout%s. Rows x Cols: %d x %d.",
          if (!identical(new_order, current_order) &&
              isTRUE(input$opt_compact_order)) " + compact order" else "",
          new_r, new_c
        )
        showNotification(msg, type = "message")
        debug_log(paste("Optimise applied:", msg), 1)
      } else {
        showNotification("No more compact layout found. Nothing changed.",
                         type = "message")
        debug_log("Optimise: no improvement; nothing changed", 1)
      }
    })

    # Registry: register remove-button observer once per plot ID.
    local({
      registered <- character(0)
      observe({
        ids <- rv$gridplot_order %||% character(0)
        if (!length(ids)) return()
        new_ids <- setdiff(ids, registered)
        if (!length(new_ids)) return()
        for (id in new_ids) {
          local({
            local_id  <- id
            local_btn <- paste0("remove_", gsub("[^[:alnum:]_]+", "_", id))
            observeEvent(input[[local_btn]], ignoreInit = TRUE, {
              if (!(local_id %in% rv$gridplot_order)) return()
              tryCatch({
                rv$gridplot_selection[[local_id]] <- NULL
                rv$gridplot_selection <- rv$gridplot_selection
                rv$gridplot_order     <-
                  setdiff(rv$gridplot_order, local_id)
                if (!is.null(rv$plot_margins[[local_id]]))
                  rv$plot_margins[[local_id]] <- NULL
                if (!is.null(rv$plot_spans[[local_id]]))
                  rv$plot_spans[[local_id]] <- NULL
                showNotification(paste("Removed", local_id, "from grid"),
                                 type = "message", duration = 2)
              }, error = function(e) {
                showNotification("Error removing plot", type = "error")
              })
            })
          })
        }
        registered <<- c(registered, new_ids)
      })
    })

    # -------------------------------------------------------------------------
    # f. Composition helpers
    # -------------------------------------------------------------------------

    current_plot <- reactiveVal(NULL)
    last_auto_render_signature <- reactiveVal(NULL)

    # Shared composition function called from both auto-update and manual paths.
    # include_map may be pre-computed (auto-update path) or derived here
    # (manual path).  rv is passed explicitly to composition functions.
    compose_from <- function(plots, settings, include_map = NULL) {
      if (length(plots) == 0) {
        return(ggplot2::ggplot() + ggplot2::theme_void() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "No plots in grid yet", size = 6))
      }

      if (is.null(names(plots))) {
        debug_log("Plots have no names; assigning from gridplot_order", 1)
        ord <- isolate(rv$gridplot_order)
        names(plots) <- if (length(ord) >= length(plots))
          ord[seq_along(plots)] else paste0("plot_", seq_along(plots))
      }

      prepared <- tryCatch(
        prepare_plots_for_grid(plots, settings, debug_log = debug_log),
        error = function(e) {
          debug_log(paste("prepare_plots_for_grid failed:", e$message), 1)
          plots
        }
      )
      if (is.null(prepared)) {
        debug_log("prepare_plots_for_grid returned NULL; using originals", 1)
        prepared <- plots
      }

      labels <- tryCatch({
        plot_names <- names(prepared)
        if (is.null(include_map))
          include_map <- get_include_map(isolate(rv), plot_names,
                                          debug_log = debug_log)
        build_labels(settings, plot_names, include_map, debug_log = debug_log)
      }, error = function(e) {
        debug_log(paste("build_labels failed:", e$message), 1)
        NULL
      })

      tryCatch(
        compose_grid(prepared, settings, labels, rv = rv,
                      debug_log = debug_log),
        error = function(e) {
          debug_log(paste("compose_grid failed:", e$message), 1)
          ggplot2::ggplot() + ggplot2::theme_void() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = paste0("Grid composition error: ",
                                              e$message),
                              size = 5, color = "red")
        }
      )
    }

    # Debounced reactive: bundles all composition inputs to prevent rapid
    # re-renders when multiple settings change within 500 ms.
    render_data <- reactive({
      plots      <- selected_plots()
      settings   <- grid_settings()
      plot_names <- names(plots)
      include_map <- if (length(plot_names) > 0)
        get_include_map(rv, plot_names, debug_log = debug_log) else NULL
      list(plots = plots, settings = settings, include_map = include_map)
    })
    render_data_d <- debounce(render_data, millis = 500)

    # Auto-update path: only active when the auto_update checkbox is checked.
    observe({
      req(isTRUE(input$auto_update))
      data <- render_data_d()
      plot_names <- names(data$plots)
      if (length(data$plots) == 0) {
        debug_log("Auto-update skipped: no plots selected", 2)
        return()
      }

      # When the debounced data lags behind a newer grid order, avoid
      # rendering the stale prefix. Read rv$gridplot_order here, inside the
      # observer's reactive context, so Shiny tracks the dependency safely.
      latest_order <- rv$gridplot_order %||% character(0)
      if (length(plot_names) > 0 && length(plot_names) < length(latest_order) &&
          identical(plot_names, latest_order[seq_along(plot_names)])) {
        debug_log("Auto-update skipped: stale debounced prefix of newer order", 2)
        return()
      }

      include_flags <- NULL
      if (!is.null(data$include_map) && length(plot_names) > 0) {
        include_flags <- data$include_map[plot_names]
      }
      signature <- list(
        plot_names = plot_names,
        nrow = data$settings$nrow,
        ncol = data$settings$ncol,
        align = data$settings$align,
        labels_mode = data$settings$labels_mode,
        force_legend_position = data$settings$force_legend_position,
        include_flags = include_flags
      )
      if (identical(signature, last_auto_render_signature())) {
        debug_log("Auto-update skipped: render signature unchanged", 2)
        return()
      }

      debug_log("Auto-update render triggered (debounced)", 1)
      current_plot(compose_from(data$plots, data$settings, data$include_map))
      last_auto_render_signature(signature)
    })

    # Manual render via "Create Plot" button (works in both modes).
    observeEvent(input$create_plot, {
      debug_log("Manual render triggered via Create Plot", 1)
      current_plot(compose_from(selected_plots(), grid_settings()))
      local({
        plts <- selected_plots()
        sts  <- grid_settings()
        rows <- sts$nrow %||% NA
        cols <- sts$ncol %||% NA
        debug_log(paste("Plot grid composed:", length(plts), "plots \u2013",
                        "layout:", rows, "x", cols), level = 0)
      })
    })

    # -------------------------------------------------------------------------
    # g. Render and download handlers
    # -------------------------------------------------------------------------

    output$preview <- renderPlot({
      p <- current_plot()
      if (is.null(p)) {
        ggplot2::ggplot() + ggplot2::theme_void() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "No plots in grid yet", size = 6)
      } else p
    }, res = 96)

    output$download <- downloadHandler(
      filename = function() {
        fmt <- input$downloadFormat
        if (is.null(fmt) || !nzchar(fmt)) fmt <- "png"
        paste0("plot_grid.", fmt)
      },
      content = function(file) {
        plots <- selected_plots()
        if (length(plots) == 0) {
          showNotification("No plots to download.", type = "error")
          debug_log("Download aborted: no plots selected", 1)
          return()
        }

        settings <- grid_settings()

        prepared <- tryCatch(
          prepare_plots_for_grid(plots, settings, debug_log = debug_log),
          error = function(e) {
            debug_log(paste("prepare_plots_for_grid failed (download):",
                             e$message), 1)
            plots
          }
        )

        plot_names  <- names(prepared) %||% names(plots)
        include_map <- tryCatch(
          get_include_map(rv, plot_names, debug_log = debug_log),
          error = function(e) {
            debug_log(paste("get_include_map failed (download):", e$message), 1)
            setNames(rep(TRUE, length(plot_names)), plot_names)
          }
        )
        labels <- tryCatch(
          build_labels(settings, plot_names, include_map,
                        debug_log = debug_log),
          error = function(e) {
            debug_log(paste("build_labels failed (download):", e$message), 1)
            NULL
          }
        )

        widthIn  <- { x <- input$plotWidthInch;
                       if (is.null(x)) 14 else as.numeric(x) }
        heightIn <- { x <- input$plotHeightInch;
                       if (is.null(x)) 10 else as.numeric(x) }
        res      <- { x <- input$resolution_DPI;
                       if (is.null(x)) 600 else as.integer(x) }
        fmt      <- { x <- input$downloadFormat;
                       if (is.null(x) || !nzchar(x)) "png" else x }

        debug_log(
          paste("Download:", fmt, "| size:", widthIn, "x", heightIn,
                "in @", res, "ppi"),
          2
        )

        switch(fmt,
          png  = grDevices::png (file, width = widthIn, height = heightIn,
                                 units = "in", res = res),
          jpeg = grDevices::jpeg(file, width = widthIn, height = heightIn,
                                 units = "in", res = res),
          tiff = grDevices::tiff(file, width = widthIn, height = heightIn,
                                 units = "in", res = res),
          svg  = {
            if (!requireNamespace("svglite", quietly = TRUE))
              stop("svglite not installed")
            svglite::svglite(file, width = widthIn, height = heightIn)
          },
          pdf  = grDevices::pdf(file, width = widthIn, height = heightIn),
               grDevices::png(file, width = widthIn, height = heightIn,
                               units = "in", res = res)
        )
        on.exit(grDevices::dev.off(), add = TRUE)

        # Compose after opening the target graphics device. Some aligned grobs
        # (notably enrichment maps with legends/text) depend on device metrics;
        # composing on the target device keeps downloads aligned with preview.
        p <- tryCatch(
          compose_grid(prepared, settings, labels, rv = rv,
                        debug_log = debug_log),
          error = function(e) {
            debug_log(paste("compose_grid failed (download):", e$message), 1)
            NULL
          }
        )

        if (is.null(p)) {
          showNotification("Could not compose the grid for download.",
                           type = "error")
          return()
        }

        print(p)
        debug_log("Download completed", 2)
      }
    )

    # -------------------------------------------------------------------------
    # h. Session cleanup
    # -------------------------------------------------------------------------
    cleanup_manager$register_module("Grid", function() {
      debug_log("Executing Grid module cleanup", 2)
      rv$gridplot_selection <- list()
      rv$gridplot_order     <- character(0)
      debug_log("Grid module cleanup completed", 2)
    })

    debug_log("GRID module server initialised", 1)
  })
}
