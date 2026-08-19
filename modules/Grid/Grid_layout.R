# modules/Grid/Grid_layout.R
#
# Purpose:
#   Manages the plot selection registry, label generation, span/margin state,
#   and the grid layout optimisation algorithms for the Grid module.
#
# Architecture:
#   This file is a pure utility layer with no Shiny reactivity.  All functions
#   are loaded into modEnv by Grid_module.R and called from the server in that
#   file.  No server logic lives here.
#
# Structure:
#   1. Plot-selection helpers (add, remove, clear, reorder, blanks)
#   2. Label and include-map helpers
#   3. Span and margin state utilities (ensure, clamp)
#   4. In-order vector helper (.move_swap)
#   5. Layout optimisation (compute_optimal_grid_layout and sub-functions)
#
# Future developers:
#   - All functions that emit debug output accept `debug_log` as an explicit
#     parameter with a silent no-op default so they are safe to call outside
#     the server context (e.g. from other modules that only pass rv).
#   - The `%||%` operator is defined at the bottom of this file so the module
#     works standalone; Grid_module.R also defines it in the server closure.
#   - Do not add server or reactive logic here.

# ---------------------------------------------------------------------------
# 1. Plot-selection helpers
# ---------------------------------------------------------------------------

add_to_grid <- function(rv, id, plot, label = NULL, source = NULL,
                        source_plot_id = NULL,
                        debug_log = function(...) invisible(NULL)) {
  selection <- rv$gridplot_selection %||% list()
  order     <- rv$gridplot_order     %||% character(0)
  spans     <- rv$plot_spans         %||% list()
  margins   <- rv$plot_margins       %||% list()

  if (!inherits(plot, "ggplot")) {
    showNotification("Only ggplot objects are supported for the plot grid.",
                     type = "error")
    return(invisible(FALSE))
  }

  entry <- list(
    plot          = plot,
    label         = label,
    source        = source,
    source_plot_id = as.character(source_plot_id %||% label %||% id)[1L],
    type          = "ggplot",
    include_label = TRUE,
    added_at      = Sys.time()
  )
  selection[[id]] <- entry
  if (!(id %in% order))
    order <- c(order, id)
  if (is.null(spans[[id]]))
    spans[[id]] <- list(colspan = 1L, rowspan = 1L)
  if (is.null(margins[[id]]))
    margins[[id]] <- list(top = 0, right = 0, bottom = 0, left = 0)

  rv$plot_spans         <- spans
  rv$plot_margins       <- margins
  rv$gridplot_order     <- order
  rv$gridplot_selection <- selection
  debug_log(paste("Added plot to grid:", id), 1)
  invisible(TRUE)
}

remove_from_grid <- function(rv, id) {
  if (is.null(rv$gridplot_selection) || is.null(rv$gridplot_order))
    return(invisible(FALSE))
  if (id %in% names(rv$gridplot_selection)) {
    rv$gridplot_selection[[id]] <- NULL
    rv$gridplot_selection <- rv$gridplot_selection
  }
  if (id %in% rv$gridplot_order)
    rv$gridplot_order <- rv$gridplot_order[rv$gridplot_order != id]
  if (!is.null(rv$plot_margins) && id %in% names(rv$plot_margins))
    rv$plot_margins[[id]] <- NULL
  if (!is.null(rv$plot_spans)   && id %in% names(rv$plot_spans))
    rv$plot_spans[[id]] <- NULL
  invisible(TRUE)
}

clear_grid <- function(rv) {
  rv$gridplot_selection <- list()
  rv$gridplot_order     <- character(0)
  invisible(TRUE)
}

reorder_grid <- function(rv, new_order) {
  keep <- new_order[new_order %in% names(rv$gridplot_selection)]
  rv$gridplot_order <- keep
  invisible(TRUE)
}

# Generate a unique blank-plot identifier that does not collide with existing IDs.
generate_blank_id <- function(rv, debug_log = function(...) invisible(NULL)) {
  base     <- "blank_"
  i        <- 1L
  existing <- names(rv$gridplot_selection %||% list())
  while (paste0(base, i) %in% existing) i <- i + 1L
  id <- paste0(base, i)
  debug_log(paste("Generated blank id:", id), 2)
  id
}

# Add a transparent empty plot into the selection so the user can insert
# visual gaps in the grid.
add_blank_to_grid <- function(rv, id = NULL, label = NULL,
                              debug_log = function(...) invisible(NULL)) {
  selection <- rv$gridplot_selection %||% list()
  order     <- rv$gridplot_order     %||% character(0)
  spans     <- rv$plot_spans         %||% list()
  margins   <- rv$plot_margins       %||% list()

  if (is.null(id)) id <- generate_blank_id(rv, debug_log)
  p <- create_empty_plot()
  entry <- list(
    plot          = p,
    label         = label %||% "",
    source        = "blank",
    type          = "blank",
    include_label = TRUE,
    added_at      = Sys.time()
  )
  selection[[id]] <- entry
  if (!(id %in% order))
    order <- c(order, id)
  if (is.null(spans[[id]]))
    spans[[id]] <- list(colspan = 1L, rowspan = 1L)
  if (is.null(margins[[id]]))
    margins[[id]] <- list(top = 0, right = 0, bottom = 0, left = 0)

  rv$plot_spans         <- spans
  rv$plot_margins       <- margins
  rv$gridplot_order     <- order
  rv$gridplot_selection <- selection
  debug_log(paste("Added blank to grid:", id), 1)
  invisible(TRUE)
}

# Minimal blank ggplot used for empty cells and blank entries.
create_empty_plot <- function() {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin        = ggplot2::margin(0, 0, 0, 0),
      panel.background   = ggplot2::element_blank(),
      plot.background    = ggplot2::element_blank()
    )
}

# ---------------------------------------------------------------------------
# 2. Label and include-map helpers
# ---------------------------------------------------------------------------

# Build the label vector for cowplot::plot_grid.
# plot_names : character vector matching the order of prepared plots
# include_map: named logical; FALSE entries get an empty-string label
build_labels <- function(settings, plot_names, include_map = NULL,
                         debug_log = function(...) invisible(NULL)) {
  mode <- settings$labels_mode
  if (is.null(mode) || mode == "none") return(NULL)
  n <- length(plot_names)
  if (n == 0) return(NULL)

  if (is.null(include_map) || !is.logical(include_map) ||
      length(include_map) != n) {
    include_map <- rep(TRUE, n)
    names(include_map) <- plot_names
  }
  to_label_idx <- which(include_map)
  k <- length(to_label_idx)
  if (k == 0) {
    labs <- rep("", n)
    names(labs) <- plot_names
    return(labs)
  }

  base <- switch(
    mode,
    auto_letters = LETTERS[seq_len(k)],
    auto_numbers = as.character(seq_len(k)),
    custom = {
      raw <- settings$labels_custom
      if (is.null(raw)) return(NULL)
      v <- trimws(strsplit(raw, ",")[[1]])
      if (length(v) == 0) return(NULL)
      if (length(v) < k) v <- c(v, rep("", k - length(v)))
      if (length(v) > k) v <- v[seq_len(k)]
      v
    },
    NULL
  )
  if (is.null(base)) return(NULL)

  labs <- rep("", n)
  labs[to_label_idx] <- base
  names(labs) <- plot_names
  debug_log(
    paste("Labels aligned:",
          paste(sprintf("%s='%s'", names(labs), labs), collapse = ", ")),
    2
  )
  labs
}

# Build the include map for the current plot order from rv state.
get_include_map <- function(rv, plot_names,
                            debug_log = function(...) invisible(NULL)) {
  if (length(plot_names) == 0)
    return(setNames(logical(0), character(0)))
  include        <- rep(TRUE, length(plot_names))
  names(include) <- plot_names
  sel <- rv$gridplot_selection %||% list()
  for (nm in plot_names) {
    if (!is.null(sel[[nm]]) && !is.null(sel[[nm]]$include_label))
      include[nm] <- isTRUE(sel[[nm]]$include_label)
  }
  debug_log(
    paste("Include map:",
          paste(sprintf("%s=%s", names(include), include), collapse = ", ")),
    2
  )
  include
}

# ---------------------------------------------------------------------------
# 3. Span and margin state utilities
# ---------------------------------------------------------------------------

# Ensure a plot_spans entry exists and its fields are integers.
ensure_span_entry <- function(rv, id) {
  if (is.null(rv$plot_spans[[id]])) {
    rv$plot_spans[[id]] <- list(colspan = 1L, rowspan = 1L)
  } else {
    rv$plot_spans[[id]]$colspan <-
      as.integer(rv$plot_spans[[id]]$colspan %||% 1L)
    rv$plot_spans[[id]]$rowspan <-
      as.integer(rv$plot_spans[[id]]$rowspan %||% 1L)
  }
}

# Reduce any spans that exceed the current grid dimensions.
clamp_spans_to_grid <- function(rv, nrow, ncol,
                                debug_log = function(...) invisible(NULL)) {
  if (is.null(rv$plot_spans) || length(rv$plot_spans) == 0)
    return(invisible())
  changed <- FALSE
  for (id in names(rv$plot_spans)) {
    sp    <- rv$plot_spans[[id]]
    old_c <- sp$colspan
    old_r <- sp$rowspan
    sp$colspan <- max(1L, min(as.integer(ncol), as.integer(old_c %||% 1L)))
    sp$rowspan <- max(1L, min(as.integer(nrow), as.integer(old_r %||% 1L)))
    if (!identical(sp$colspan, old_c) || !identical(sp$rowspan, old_r))
      changed <- TRUE
    rv$plot_spans[[id]] <- sp
  }
  if (changed) debug_log("Spans clamped to new grid size", 1)
  invisible()
}

# ---------------------------------------------------------------------------
# 4. Vector helper
# ---------------------------------------------------------------------------

# Swap an element with its immediate neighbour in direction "up" or "down".
.move_swap <- function(vec, idx_from, direction = c("up", "down")) {
  direction <- match.arg(direction)
  n <- length(vec)
  if (n <= 1 || idx_from < 1 || idx_from > n) return(vec)
  if (direction == "up") {
    if (idx_from == 1) return(vec)
    idx_to <- idx_from - 1L
  } else {
    if (idx_from == n) return(vec)
    idx_to <- idx_from + 1L
  }
  tmp           <- vec[idx_to]
  vec[idx_to]   <- vec[idx_from]
  vec[idx_from] <- tmp
  vec
}

# ---------------------------------------------------------------------------
# 5. Layout optimisation
# ---------------------------------------------------------------------------

# Compute the minimum number of grid rows needed for a given column count
# when placing spans sequentially without backfilling earlier rows.
min_rows_for_cols <- function(spans, ncol, order_ids, max_rows = 200L) {
  rows_used <- simulate_rows_no_backfill(spans, ncol, order_ids)
  min(rows_used, as.integer(max_rows %||% 200L))
}

# Heuristic compact reorder: First-Fit-Decreasing by colspan, stable within
# equal-width bins.  Returns a new order vector of plot IDs.
reorder_compact_fit <- function(spans, order_ids, ncol) {
  if (is.null(order_ids) || !length(order_ids)) order_ids <- names(spans)
  if (!length(order_ids)) return(character(0))

  idx_map <- setNames(seq_along(order_ids), order_ids)
  widths  <- vapply(order_ids, function(id) {
    sp <- spans[[id]] %||% list()
    max(1L, min(as.integer(sp$colspan %||% 1L), as.integer(ncol %||% 1L)))
  }, integer(1L))

  ord_sorted <- order(-widths, idx_map)
  items      <- order_ids[ord_sorted]
  w          <- widths[ord_sorted]

  rows      <- list()
  row_space <- c()
  for (i in seq_along(items)) {
    placed <- FALSE
    for (r in seq_along(rows)) {
      if ((row_space[r] + w[i]) <= ncol) {
        rows[[r]]    <- c(rows[[r]], items[i])
        row_space[r] <- row_space[r] + w[i]
        placed       <- TRUE
        break
      }
    }
    if (!placed) {
      rows[[length(rows) + 1L]]      <- items[i]
      row_space[length(row_space) + 1L] <- w[i]
    }
  }
  unlist(rows, use.names = FALSE)
}

# Simulate sequential placement (no backfill into earlier rows) and return
# the number of rows actually used.
simulate_rows_no_backfill <- function(spans, ncol, order_ids) {
  if (is.null(spans) || !length(spans)) return(0L)
  ncol <- as.integer(max(1L, ncol))
  if (is.null(order_ids) || !length(order_ids)) order_ids <- names(spans)
  if (!length(order_ids)) return(0L)

  # Dynamic occupancy matrix: list of integer vectors of length ncol.
  # 0 = free, positive integer = occupied by that plot index.
  occ          <- list(integer(ncol))
  max_row_used <- 0L
  cur_r        <- 1L
  cur_c        <- 1L
  plot_idx     <- 1L

  ensure_rows <- function(rows_needed) {
    while (length(occ) < rows_needed)
      occ[[length(occ) + 1L]] <<- integer(ncol)
  }

  for (id in order_ids) {
    sp <- spans[[id]] %||% list()
    cs <- as.integer(sp$colspan %||% 1L)
    rs <- as.integer(sp$rowspan %||% 1L)
    if (!is.finite(cs) || cs < 1L) cs <- 1L
    if (!is.finite(rs) || rs < 1L) rs <- 1L
    cs <- min(cs, ncol)

    placed  <- FALSE
    r       <- cur_r
    c_start <- cur_c

    repeat {
      ensure_rows(r + rs - 1L)

      for (c in c_start:ncol) {
        end_r <- r + rs - 1L
        end_c <- c + cs - 1L
        if (end_c > ncol) break

        fits <- TRUE
        for (rr in r:end_r) {
          row_vec <- occ[[rr]]
          if (any(row_vec[c:end_c] != 0L)) { fits <- FALSE; break }
        }
        if (!fits) next

        # Mark cells as occupied.
        for (rr in r:end_r) occ[[rr]][c:end_c] <- plot_idx
        max_row_used <- max(max_row_used, end_r)
        cur_r <- r
        cur_c <- end_c + 1L
        if (cur_c > ncol) { cur_r <- cur_r + 1L; cur_c <- 1L }
        placed <- TRUE
        break
      }

      if (placed) break

      # Advance to the next row, starting from column 1.
      r       <- r + 1L
      c_start <- 1L

      # Safety limit to prevent infinite loops on degenerate inputs.
      if (r > 10000L) break
    }

    plot_idx <- plot_idx + 1L
  }

  as.integer(max(1L, max_row_used))
}

# Lower bound on the total number of grid cells required by all spans.
compute_total_span_area <- function(spans) {
  if (is.null(spans) || !length(spans)) return(0L)
  sum(vapply(names(spans), function(id) {
    sp <- spans[[id]]
    cs <- as.integer(sp$colspan %||% 1L)
    rs <- as.integer(sp$rowspan %||% 1L)
    max(1L, cs) * max(1L, rs)
  }, integer(1L)))
}

# Fast greedy packability check: can all spans be placed in an nrow x ncol
# grid in the given order without backfilling?
can_pack_spans_greedy <- function(spans, nrow, ncol, order_ids) {
  if (nrow < 1L || ncol < 1L) return(FALSE)
  if (is.null(order_ids) || !length(order_ids)) order_ids <- names(spans)
  if (!length(order_ids)) return(TRUE)
  total_area <- compute_total_span_area(spans)
  if (nrow * ncol < total_area) return(FALSE)

  mat <- matrix(0L, nrow = nrow, ncol = ncol)
  idx <- 1L

  for (id in order_ids) {
    sp    <- spans[[id]] %||% list()
    cs    <- max(1L, min(as.integer(sp$colspan %||% 1L), ncol))
    rs    <- max(1L, min(as.integer(sp$rowspan %||% 1L), nrow))
    placed <- FALSE

    for (r in 1:nrow) {
      if (placed) break
      for (c in 1:ncol) {
        end_r <- r + rs - 1L
        end_c <- c + cs - 1L
        if (end_r > nrow || end_c > ncol) next
        block <- mat[r:end_r, c:end_c, drop = FALSE]
        if (any(block != 0L)) next
        mat[r:end_r, c:end_c] <- idx
        placed <- TRUE
        break
      }
    }
    if (!placed) return(FALSE)
    idx <- idx + 1L
  }
  TRUE
}

# Find the row x column layout that best fits n plots (or span area) while
# matching the container aspect ratio and minimising empty cells.
compute_optimal_grid_layout <- function(n,
                                        container_ratio  = 1.4,
                                        max_rows         = 10L,
                                        max_cols         = 10L,
                                        strategy         = c("balanced"),
                                        prefer_fewer_empty = TRUE,
                                        spans            = NULL,
                                        order_ids        = NULL,
                                        enforce_pack     = FALSE,
                                        debug_log        = function(...) invisible(NULL)) {
  strategy         <- match.arg(strategy)
  container_ratio  <- suppressWarnings(as.numeric(container_ratio %||% 1.4))
  if (!is.finite(container_ratio) || container_ratio <= 0) container_ratio <- 1.4

  have_spans <- enforce_pack && !is.null(spans) && length(spans) > 0
  if (have_spans) {
    n <- length(spans)
  } else {
    n <- as.integer(n %||% 0L)
  }
  if (is.na(n) || n <= 0L) {
    debug_log("compute_optimal_grid_layout: n <= 0, fallback 2x2", 1)
    return(list(nrow = 2L, ncol = 2L, score = Inf, empty = 0L, pack_ok = TRUE))
  }

  max_rows  <- as.integer(max(1L, min(50L, max_rows %||% 10L)))
  max_cols  <- as.integer(max(1L, min(50L, max_cols %||% 10L)))
  w_empty   <- if (prefer_fewer_empty) 2.0 else 1.0
  w_ar      <- 1.0
  w_skew    <- 0.2
  min_cells <- if (have_spans) compute_total_span_area(spans) else n

  best          <- list(nrow = 2L, ncol = 2L, score = Inf, empty = Inf, pack_ok = FALSE)
  any_feasible  <- FALSE

  for (r in 1:max_rows) {
    for (c in 1:max_cols) {
      cells <- r * c
      if (cells < min_cells) next

      pack_ok <- TRUE
      if (have_spans) {
        pack_ok <- can_pack_spans_greedy(spans, r, c, order_ids)
        if (!pack_ok) next
      }

      empty      <- max(0L, cells - min_cells)
      ratio_grid <- c / r
      ar_pen     <- abs(log(ratio_grid / container_ratio))
      skew_pen   <- abs(r - c) / max(r, c)
      score      <- w_empty * empty + w_ar * ar_pen + w_skew * skew_pen

      is_better <- (score < best$score) ||
        (abs(score - best$score) < 1e-9 && (empty < best$empty)) ||
        (abs(score - best$score) < 1e-9 && empty == best$empty &&
           skew_pen < abs(best$nrow - best$ncol) / max(best$nrow, best$ncol)) ||
        (abs(score - best$score) < 1e-9 && empty == best$empty &&
           abs(skew_pen - abs(best$nrow - best$ncol) / max(best$nrow, best$ncol)) < 1e-9 &&
           abs(ratio_grid - container_ratio) < abs(best$ncol / best$nrow - container_ratio))

      if (is_better) {
        best <- list(nrow = as.integer(r), ncol = as.integer(c),
                     score = score, empty = empty, pack_ok = pack_ok)
        any_feasible <- TRUE
      }
    }
  }

  if (have_spans && !any_feasible) {
    debug_log(
      "No packable layout found within limits; falling back to area-based suggestion", 1
    )
    fallback <- list(nrow = 2L, ncol = 2L, score = Inf, empty = Inf, pack_ok = FALSE)
    for (r in 1:max_rows) {
      for (c in 1:max_cols) {
        cells <- r * c
        if (cells < min_cells) next
        empty      <- cells - min_cells
        ratio_grid <- c / r
        ar_pen     <- abs(log(ratio_grid / container_ratio))
        skew_pen   <- abs(r - c) / max(r, c)
        score      <- w_empty * empty + w_ar * ar_pen + w_skew * skew_pen
        if (score < fallback$score) {
          fallback <- list(nrow = as.integer(r), ncol = as.integer(c),
                           score = score, empty = empty, pack_ok = FALSE)
        }
      }
    }
    return(fallback)
  }

  best
}

# ---------------------------------------------------------------------------
# Shared utilities (also used by Grid_composition.R)
# ---------------------------------------------------------------------------

# Null-coalescing operator: return b when a is NULL or length-zero.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Safe no-op fallback for debug_log when running outside the server context.
if (!exists("debug_log", inherits = FALSE)) {
  debug_log <- function(...) invisible(NULL)
}
