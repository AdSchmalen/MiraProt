# modules/Grid/Grid_composition.R
#
# Purpose:
#   Assembles individual ggplot objects into the final grid using cowplot.
#   Handles composite-plot detection, alignment compatibility groups,
#   span-based canvas placement, and margin management.
#
# Architecture:
#   Pure utility layer with no Shiny reactivity.  Loaded into modEnv by
#   Grid_module.R and called from compose_from() in the server.  No server
#   logic lives here.
#
# Structure:
#   1. Plot preparation helpers (validate_margin_value, prepare_plots_for_grid,
#      apply_per_plot_margins, create_empty_plot)
#   2. Composite-plot detection (is_cowplot_composite, normalize helpers)
#   3. Alignment (align_plots_if_requested)
#   4. Span-layout primitives (compute_span_coordinates,
#      create_span_layout_matrix_fixed)
#   5. Composition entry points (compose_grid_with_spans_safe, compose_grid)
#
# Future developers:
#   - compose_grid() is the primary entry point called from Grid_module.R.
#   - All functions accept debug_log as an explicit parameter (no-op default).
#   - compose_grid() and align_plots_if_requested() accept an optional rv
#     parameter for source-based UpSet/alignment exclusion.  Pass rv from
#     the server closure for correct behaviour; omit when calling from
#     contexts where rv is unavailable.
#   - Do not add server or reactive logic here.

# ---------------------------------------------------------------------------
# 1. Plot preparation helpers
# ---------------------------------------------------------------------------

validate_margin_value <- function(value, name, default = 10,
                                  debug_log = function(...) invisible(NULL)) {
  if (is.null(value)) {
    debug_log(paste("Margin", name, "is NULL, using default:", default), 2)
    return(default)
  }
  if (!is.numeric(value)) {
    debug_log(paste("Margin", name, "is not numeric:", value,
                    "using default:", default), 2)
    return(default)
  }
  if (is.na(value) || !is.finite(value)) {
    debug_log(paste("Margin", name, "is NA or infinite:", value,
                    "using default:", default), 2)
    return(default)
  }
  validated <- max(0, min(100, as.numeric(value)))
  if (validated != value)
    debug_log(paste("Margin", name, "clamped from", value, "to", validated), 2)
  validated
}

# Apply global margins and optionally hide plot titles/subtitles/captions.
# Per-plot offset margins are NOT applied here; they are applied after
# alignment by apply_per_plot_margins() so that cowplot::align_plots cannot
# equalise or strip the individual subplot differences.
prepare_plots_for_grid <- function(plots, settings,
                                   debug_log = function(...) invisible(NULL)) {
  if (!length(plots)) return(plots)
  hide_titles <- isTRUE(settings$hide_titles)
  margins     <- settings$margins %||% list(top = 10, right = 10, bottom = 10, left = 10)

  validated <- list(
    top    = validate_margin_value(margins$top,    "top",    10, debug_log),
    right  = validate_margin_value(margins$right,  "right",  10, debug_log),
    bottom = validate_margin_value(margins$bottom, "bottom", 10, debug_log),
    left   = validate_margin_value(margins$left,   "left",   10, debug_log)
  )

  out <- lapply(seq_along(plots), function(i) {
    p <- plots[[i]]
    if (!inherits(p, "ggplot")) return(p)
    if (hide_titles)
      p <- p + ggplot2::theme(
        plot.title    = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_blank(),
        plot.caption  = ggplot2::element_blank()
      )
    p + ggplot2::theme(
      plot.margin = ggplot2::margin(
        t    = validated$top,
        r    = validated$right,
        b    = validated$bottom,
        l    = validated$left,
        unit = "pt"
      )
    )
  })

  # Preserve plot names for downstream span matching, label inclusion, and
  # source detection.
  names(out) <- names(plots)
  out
}

# Apply per-plot margin offsets AFTER alignment.
# Reads rv$plot_margins offsets and adds them on top of whatever margin was
# already set.  Supports negative offsets for tightening whitespace.
apply_per_plot_margins <- function(plots, settings,
                                   debug_log = function(...) invisible(NULL)) {
  plot_margins <- settings$plot_margins %||% list()
  if (length(plot_margins) == 0) return(plots)
  plot_names <- names(plots)

  out <- lapply(seq_along(plots), function(i) {
    nm <- if (!is.null(plot_names) && i <= length(plot_names)) plot_names[[i]] else NULL
    if (is.null(nm)) return(plots[[i]])
    pm <- plot_margins[[nm]]
    if (is.null(pm)) return(plots[[i]])

    t_off <- suppressWarnings(as.numeric(pm$top    %||% 0))
    r_off <- suppressWarnings(as.numeric(pm$right  %||% 0))
    b_off <- suppressWarnings(as.numeric(pm$bottom %||% 0))
    l_off <- suppressWarnings(as.numeric(pm$left   %||% 0))
    if (!is.finite(t_off)) t_off <- 0
    if (!is.finite(r_off)) r_off <- 0
    if (!is.finite(b_off)) b_off <- 0
    if (!is.finite(l_off)) l_off <- 0
    if (t_off == 0 && r_off == 0 && b_off == 0 && l_off == 0) return(plots[[i]])

    p <- plots[[i]]

    if (.is_enrichment_map_alignable(p)) {
      stretched <- .wrap_enrichment_map_for_margin_stretch(
        p, t_off = t_off, r_off = r_off, b_off = b_off, l_off = l_off,
        nm = nm, debug_log = debug_log
      )
      if (!is.null(stretched)) return(stretched)
    }

    if (inherits(p, "ggplot")) {
      base_t <- base_r <- base_b <- base_l <- 0
      tryCatch({
        m <- p$theme$plot.margin
        if (!is.null(m) && length(m) >= 4) {
          safe_num <- function(x) { v <- as.numeric(x); if (is.finite(v)) v else 0 }
          base_t <- safe_num(m[1])
          base_r <- safe_num(m[2])
          base_b <- safe_num(m[3])
          base_l <- safe_num(m[4])
        }
      }, error = function(e) NULL)

      new_t <- max(-200, min(500, base_t + t_off))
      new_r <- max(-200, min(500, base_r + r_off))
      new_b <- max(-200, min(500, base_b + b_off))
      new_l <- max(-200, min(500, base_l + l_off))

      debug_log(sprintf(
        "Per-plot margin for %s: base=(%g,%g,%g,%g) offset=(%g,%g,%g,%g) final=(%g,%g,%g,%g)",
        nm, base_t, base_r, base_b, base_l,
        t_off, r_off, b_off, l_off,
        new_t, new_r, new_b, new_l
      ), 2)

      return(p + ggplot2::theme(
        plot.margin = ggplot2::margin(t = new_t, r = new_r, b = new_b, l = new_l,
                                      unit = "pt")
      ))
    }

    # cowplot::align_plots() returns aligned gtables. Apply offsets to those
    # aligned objects by adding outer gtable rows/columns, so per-plot margins
    # are still based on the aligned plot rather than feeding back into
    # alignment. Negative units are intentionally preserved to support the
    # existing tightening workflow.
    if (inherits(p, "gtable") && requireNamespace("gtable", quietly = TRUE)) {
      debug_log(sprintf(
        "Per-plot gtable margin for %s: offset=(%g,%g,%g,%g)",
        nm, t_off, r_off, b_off, l_off
      ), 2)
      return(tryCatch({
        g <- p
        if (.is_enrichment_map_alignable(g)) {
          g <- .make_enrichment_map_gtable_alignable(g, nm, debug_log)
        }
        if (t_off != 0) g <- gtable::gtable_add_rows(g, grid::unit(t_off, "pt"), pos = 0)
        if (b_off != 0) g <- gtable::gtable_add_rows(g, grid::unit(b_off, "pt"), pos = -1)
        if (l_off != 0) g <- gtable::gtable_add_cols(g, grid::unit(l_off, "pt"), pos = 0)
        if (r_off != 0) g <- gtable::gtable_add_cols(g, grid::unit(r_off, "pt"), pos = -1)
        g
      }, error = function(e) {
        debug_log(paste("Per-plot gtable margin failed for", nm, ":", e$message), 1)
        p
      }))
    }

    p
  })

  names(out) <- names(plots)
  out
}

# ---------------------------------------------------------------------------
# 2. Composite-plot detection
# ---------------------------------------------------------------------------

.is_enrichment_map_alignable <- function(plot) {
  inherits(plot, "gsea_enrichment_map_alignable") ||
    inherits(plot, "go_enrichment_map_alignable")
}

.is_gsea_running_score_alignable <- function(plot) {
  inherits(plot, "gsea_running_score_alignable")
}

.get_gsea_running_score_plotlist <- function(plot) {
  if (!.is_gsea_running_score_alignable(plot)) return(NULL)
  plotlist <- attr(plot, "gsea_running_score_plotlist", exact = TRUE)
  if (is.list(plotlist) && length(plotlist) > 0) plotlist else NULL
}

.get_gsea_running_score_rel_heights <- function(plot, n_parts) {
  rel_heights <- attr(plot, "gsea_running_score_rel_heights", exact = TRUE)
  if (is.numeric(rel_heights) && length(rel_heights) == n_parts) return(rel_heights)
  rep(1, n_parts)
}

.get_gsea_running_score_panel_gap <- function(plot) {
  gap <- attr(plot, "gsea_running_score_panel_gap", exact = TRUE)
  gap <- suppressWarnings(as.numeric(gap %||% 1.5))
  if (is.finite(gap) && gap >= 0) gap else 1.5
}

.compose_gsea_running_score_parts <- function(parts, rel_heights, panel_gap,
                                              align_mode, axis_mode) {
  if (length(parts) == 2L && requireNamespace("patchwork", quietly = TRUE)) {
    return(
      patchwork::wrap_elements(full = parts[[1]]) /
        patchwork::plot_spacer() /
        patchwork::wrap_elements(full = parts[[2]]) +
        patchwork::plot_layout(
          heights = grid::unit(c(rel_heights[[1]], panel_gap, rel_heights[[2]]),
                               c("null", "pt", "null"))
        )
    )
  }

  cowplot::plot_grid(
    plotlist    = parts,
    ncol        = 1,
    align       = align_mode,
    axis        = axis_mode,
    rel_heights = rel_heights,
    greedy      = TRUE
  )
}

.make_enrichment_map_gtable_alignable <- function(plot, nm = "", debug_log = function(...) invisible(NULL)) {
  if (!.is_enrichment_map_alignable(plot) || !inherits(plot, "gtable"))
    return(plot)

  tryCatch({
    g <- plot
    # enrichplot::emapplot/ggraph can carry fixed/respected panel geometry.
    # After cowplot has aligned the full gtable (including any legend columns),
    # only relax the enrichment-map gtable so its map panel can use the same
    # horizontal panel span as regular ggplots.  Do not change other plots or
    # ask regular ggplots to reserve special enrichment-map space.
    g$respect <- FALSE

    panel_cols <- unique(g$layout$l[grepl("^panel", g$layout$name)])
    if (length(panel_cols) > 0 && requireNamespace("grid", quietly = TRUE)) {
      g$widths[panel_cols] <- grid::unit(rep(1, length(panel_cols)), "null")
      debug_log(
        paste("Relaxed enrichment map panel widths for alignment:", nm),
        2
      )
    }
    g
  }, error = function(e) {
    debug_log(paste("Could not relax enrichment map alignment for", nm, ":", e$message), 1)
    plot
  })
}

.wrap_enrichment_map_for_margin_stretch <- function(plot, t_off, r_off, b_off, l_off,
                                                    nm, debug_log) {
  if (!requireNamespace("cowplot", quietly = TRUE)) return(NULL)

  tryCatch({
    plot_grob <- if (inherits(plot, "ggplot")) {
      # Enrichment maps from enrichplot/ggraph use fixed coordinates. For grid
      # margin offsets, convert to a grob with non-clipping cartesian coords so
      # left/right offsets can expand the map beyond its original cell bounds.
      ggplot2::ggplotGrob(plot + ggplot2::coord_cartesian(clip = "off"))
    } else {
      plot
    }

    if (inherits(plot_grob, "gtable")) plot_grob$respect <- FALSE

    debug_log(sprintf(
      "Stretching enrichment map %s with viewport offsets=(%g,%g,%g,%g)",
      nm, t_off, r_off, b_off, l_off
    ), 2)

    stretched_grob <- grid::grobTree(
      plot_grob,
      vp = grid::viewport(
        x      = grid::unit(l_off, "pt"),
        y      = grid::unit(b_off, "pt"),
        width  = grid::unit(1, "npc") - grid::unit(l_off + r_off, "pt"),
        height = grid::unit(1, "npc") - grid::unit(t_off + b_off, "pt"),
        just   = c("left", "bottom"),
        clip   = "off"
      )
    )

    cowplot::ggdraw() +
      cowplot::draw_grob(stretched_grob, x = 0, y = 0, width = 1, height = 1) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")
      )
  }, error = function(e) {
    debug_log(paste("Could not stretch enrichment map margins for", nm, ":", e$message), 1)
    NULL
  })
}

# Detect whether a plot is a composite (cowplot, ComplexHeatmap, UpSet, or
# complex faceting) that should be excluded from global axis alignment.
# Always returns a non-NA logical; TRUE means "exclude from alignment".
is_cowplot_composite <- function(plot, plot_name = NULL,
                                 debug_log = function(...) invisible(NULL)) {
  plot_label <- if (!is.null(plot_name) && length(plot_name) > 0 &&
                    !is.na(plot_name[[1]]) && nzchar(as.character(plot_name[[1]]))) {
    as.character(plot_name[[1]])
  } else {
    NULL
  }
  detection_prefix <- if (!is.null(plot_label)) {
    paste0("Composite detection [", plot_label, "] ->")
  } else {
    "Composite detection ->"
  }
  detection_message <- function(...) {
    paste(detection_prefix, ...)
  }
  # Selected enrichment plots are explicitly marked as alignable by their
  # source modules. Keep this class-based escape hatch narrow so the generic
  # composite safeguards still protect unrelated faceted/composite plots.
  if (inherits(plot, "gsea_enrichment_dotplot_facet_managed") ||
      inherits(plot, "gsea_enrichment_dotplot_composite") ||
      .is_gsea_running_score_alignable(plot) ||
      .is_enrichment_map_alignable(plot)) {
    debug_log(detection_message("managed enrichment plot: allowing alignment"), 2)
    return(FALSE)
  }
  if (!inherits(plot, "ggplot")) {
    debug_log(detection_message("object is not a ggplot; treating as composite"), 2)
    return(TRUE)
  }

  # Quick heuristic: UpSet plot environment or call text
  if (!is.null(plot$call)) {
    call_text <- paste(deparse(plot$call), collapse = " ")
    if (grepl("upset", call_text, ignore.case = TRUE)) {
      debug_log(detection_message("plot call suggests UpSet; treating as composite"), 2)
      return(TRUE)
    }
  }
  if (!is.null(plot$plot_env) && !is.null(plot$plot_env$upset_data)) {
    debug_log(detection_message("plot environment contains upset_data; treating as composite"), 2)
    return(TRUE)
  }

  tryCatch({
    plot_built <- ggplot2::ggplot_build(plot)
    if (is.null(plot_built)) {
      debug_log(detection_message("ggplot_build returned NULL; treating as composite"), 2)
      return(TRUE)
    }
    plot_gtable <- ggplot2::ggplot_gtable(plot_built)
    if (is.null(plot_gtable)) {
      debug_log(detection_message("ggplot_gtable returned NULL; treating as composite"), 2)
      return(TRUE)
    }

    layout_names       <- plot_gtable$layout$name
    has_multiple_panels <- length(grep("panel", layout_names)) > 1

    has_cowplot_structure <- FALSE
    if (!is.null(plot_gtable$grobs) && length(plot_gtable$grobs) > 0) {
      grob_classes <- sapply(plot_gtable$grobs, function(g) {
        if (is.null(g)) "" else paste(class(g), collapse = "_")
      })
      has_cowplot_structure <-
        any(grepl("arrange|grid", grob_classes, ignore.case = TRUE))
    }

    has_complex_heatmap <- any(grepl("ComplexHeatmap|matrix|colorbar",
                                     layout_names, ignore.case = TRUE))
    has_upset_structure <- any(grepl("upset|intersection_size|set_size",
                                     layout_names, ignore.case = TRUE))

    has_complex_faceting <- FALSE
    if (!is.null(plot_built$data) && is.list(plot_built$data)) {
      tryCatch({
        facet_structure <- plot_built$layout
        if (!is.null(facet_structure) && is.data.frame(facet_structure)) {
          has_complex_faceting <-
            (nrow(facet_structure) > 4) ||
            (length(grep("strip", layout_names)) > 6)
        }
      }, error = function(e) {
        debug_log(detection_message("faceting check failed:", e$message), 2)
      })
    }

    # Only flag as complex-by-layers when there are many *non-standard* geom
    # types.  This avoids false positives for labelled PCA plots that have
    # many ggrepel/text/segment layers but are safely alignable.
    has_complex_layers <- FALSE
    if (!is.null(plot$layers) && length(plot$layers) > 0) {
      tryCatch({
        common_alignable <- c(
          "GeomPoint", "GeomText", "GeomLabel", "GeomTextRepel",
          "GeomLabelRepel", "GeomSegment", "GeomLine", "GeomPath",
          "GeomSmooth", "GeomBlank"
        )
        geom_types   <- sapply(plot$layers, function(layer) {
          if (is.null(layer) || is.null(layer$geom)) "unknown"
          else class(layer$geom)[1]
        })
        unique_types <- unique(geom_types)
        complex_geoms <- setdiff(unique_types, common_alignable)
        has_complex_layers <-
          (length(unique_types) > 6) && (length(complex_geoms) >= 4)
      }, error = function(e) {
        debug_log(detection_message("layer analysis failed:", e$message), 2)
      })
    }

    is_composite <- has_multiple_panels || has_cowplot_structure ||
      has_complex_heatmap || has_upset_structure ||
      has_complex_faceting || has_complex_layers

    debug_log(
      paste(detection_prefix,
            "panels:", has_multiple_panels,
            "cowplot:", has_cowplot_structure,
            "heatmap:", has_complex_heatmap,
            "upset:", has_upset_structure,
            "faceting:", has_complex_faceting,
            "layers:", has_complex_layers,
            "| result:", is_composite),
      2
    )
    as.logical(is_composite)
  }, error = function(e) {
    debug_log(detection_message("failed:", e$message,
                                "- treating as composite"), 2)
    TRUE
  })
}

# Normalise a detected composite plot for better panel spacing.
# NOTE: plot.margin is intentionally not set here; it is set by
# prepare_plots_for_grid() to avoid overwriting user-configured margins.
normalize_composite_plot <- function(plot, debug_log = function(...) invisible(NULL)) {
  if (!inherits(plot, "ggplot")) return(plot)
  tryCatch(
    plot + ggplot2::theme(
      panel.spacing = ggplot2::unit(0.5, "lines"),
      axis.text     = ggplot2::element_text(
        margin = ggplot2::margin(t = 2, b = 2, unit = "pt")),
      axis.title    = ggplot2::element_text(
        margin = ggplot2::margin(t = 5, b = 5, unit = "pt"))
    ),
    error = function(e) {
      debug_log(paste("normalize_composite_plot failed:", e$message), 2)
      plot
    }
  )
}

# Normalise a simple ggplot for consistent axis alignment.
# NOTE: plot.margin is intentionally not set here.
normalize_simple_plot <- function(plot, debug_log = function(...) invisible(NULL)) {
  if (!inherits(plot, "ggplot")) return(plot)
  tryCatch(
    plot + ggplot2::theme(
      axis.text  = ggplot2::element_text(
        margin = ggplot2::margin(t = 2, b = 2, unit = "pt")),
      axis.title = ggplot2::element_text(
        margin = ggplot2::margin(t = 5, b = 5, unit = "pt"))
    ),
    error = function(e) {
      debug_log(paste("normalize_simple_plot failed:", e$message), 2)
      plot
    }
  )
}

# ---------------------------------------------------------------------------
# 3. Alignment
# ---------------------------------------------------------------------------

# Align plots along axes using cowplot::align_plots, respecting composite-plot
# exclusion and compatibility groups.  rv is used for source-based UpSet
# exclusion; pass NULL when rv is unavailable.
align_plots_if_requested <- function(plots, settings, rv = NULL,
                                     debug_log = function(...) invisible(NULL)) {
  if (!length(plots)) return(plots)
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    debug_log("cowplot not available; skipping alignment", 1)
    return(plots)
  }

  align_mode <- settings$align %||% "none"
  if (!(align_mode %in% c("none", "h", "v", "hv"))) {
    debug_log(paste("Invalid align mode:", align_mode, "- falling back to 'none'"), 1)
    align_mode <- "none"
  }
  if (align_mode == "none") {
    debug_log("Align mode is 'none'; returning plots unchanged", 2)
    return(plots)
  }

  axis_mode <- switch(align_mode, h = "tb", v = "lr", hv = "tblr", "tblr")
  debug_log(paste("Aligning plots; mode:", align_mode, "| axis:", axis_mode), 2)

  # Exclude UpSet plots from alignment via source metadata when rv is available.
  source_exclude <- rep(FALSE, length(plots))
  if (!is.null(rv) && !is.null(names(plots)) &&
      !is.null(rv$gridplot_selection)) {
    for (i in seq_along(plots)) {
      plot_name <- names(plots)[i]
      entry     <- rv$gridplot_selection[[plot_name]]
      if (!is.null(entry)) {
        source_val <- entry$source %||% ""
        label_val  <- entry$label  %||% ""
        if (grepl("upset", source_val, ignore.case = TRUE) ||
            grepl("upset", label_val,  ignore.case = TRUE) ||
            grepl("upset", plot_name,  ignore.case = TRUE))
          source_exclude[i] <- TRUE
      }
    }
  }

  is_composite <- vapply(seq_along(plots), function(i) {
    if (isTRUE(source_exclude[i])) {
      plot_name <- if (!is.null(names(plots)) && length(names(plots)) >= i) {
        names(plots)[[i]]
      } else {
        paste0("#", i)
      }
      debug_log(
        paste("Skipping composite inspection for source-excluded plot:", plot_name),
        2
      )
      return(TRUE)
    }
    plot_name <- if (!is.null(names(plots)) && length(names(plots)) >= i) {
      names(plots)[[i]]
    } else {
      NULL
    }
    is_cowplot_composite(plots[[i]], plot_name = plot_name, debug_log = debug_log)
  }, logical(1))
  align_candidates  <- plots[!is_composite]
  candidate_names   <- names(plots)[!is_composite]

  # Detect alignment compatibility group for each candidate.
  detect_alignment_group <- function(plot_obj, plot_name) {
    if (inherits(plot_obj, "gsea_enrichment_dotplot_facet_managed") ||
        .is_gsea_running_score_alignable(plot_obj) ||
        .is_enrichment_map_alignable(plot_obj))
      return("standard")
    if (!inherits(plot_obj, "ggplot")) return("special")

    if (!is.null(rv) && !is.null(plot_name) && nzchar(plot_name) &&
        !is.null(rv$gridplot_selection[[plot_name]])) {
      source_val <- tolower(rv$gridplot_selection[[plot_name]]$source %||% "")
      if (grepl("heatmap|upset", source_val)) return("special")
    }

    is_multi_panel <- FALSE
    free_scales    <- FALSE
    tryCatch({
      pb <- ggplot2::ggplot_build(plot_obj)
      if (!is.null(pb$layout$layout) && is.data.frame(pb$layout$layout))
        is_multi_panel <- nrow(pb$layout$layout) > 1
      facet_params <- pb$layout$facet$params
      if (!is.null(facet_params)) {
        fs <- c(facet_params$free$x, facet_params$free$y)
        free_scales <- any(fs %in% TRUE)
      }
    }, error = function(e) NULL)

    if (is_multi_panel && free_scales) "facet_free" else "standard"
  }

  aligned_candidates <- align_candidates
  if (length(align_candidates) > 0) {
    groups <- vapply(seq_along(align_candidates), function(i) {
      nm <- if (!is.null(candidate_names) &&
                length(candidate_names) >= i) candidate_names[[i]] else ""
      detect_alignment_group(align_candidates[[i]], nm)
    }, character(1))

    for (grp in unique(groups)) {
      idx <- which(groups == grp)
      if (length(idx) <= 1) next
      subset <- align_candidates[idx]
      running_parts <- lapply(subset, .get_gsea_running_score_plotlist)
      has_running_parts <- vapply(running_parts, Negate(is.null), logical(1))
      aligned_subset <- tryCatch({
        # Keep aligned objects as returned by cowplot::align_plots.
        # Wrapping aligned gtables in ggdraw/draw_grob can introduce extra
        # canvas padding in multi-row layouts (most visible at the bottom).
        # Returning the aligned grobs directly preserves composite handling
        # (already excluded above) and avoids additional wrapper whitespace
        # for standard plots.
        if (any(has_running_parts)) {
          expanded <- list()
          map <- vector("list", length(subset))
          for (j in seq_along(subset)) {
            parts <- running_parts[[j]]
            if (is.null(parts)) {
              expanded[[length(expanded) + 1L]] <- subset[[j]]
              map[[j]] <- length(expanded)
            } else {
              part_idx <- integer(length(parts))
              for (k in seq_along(parts)) {
                expanded[[length(expanded) + 1L]] <- parts[[k]]
                part_idx[[k]] <- length(expanded)
              }
              map[[j]] <- part_idx
            }
          }
          aligned_expanded <- if (align_mode %in% c("v", "hv")) {
            cowplot::align_plots(
              plotlist = expanded,
              align    = "v",
              axis     = "lr"
            )
          } else {
            expanded
          }
          if (length(aligned_expanded) != length(expanded))
            stop("expanded alignment returned unexpected plot count")
          recomposed_subset <- lapply(seq_along(subset), function(j) {
            expanded_idx <- map[[j]]
            if (is.null(running_parts[[j]])) return(aligned_expanded[[expanded_idx]])
            rel_heights <- .get_gsea_running_score_rel_heights(
              subset[[j]],
              length(expanded_idx)
            )
            panel_gap <- .get_gsea_running_score_panel_gap(subset[[j]])
            recomposed <- .compose_gsea_running_score_parts(
              aligned_expanded[expanded_idx],
              rel_heights = rel_heights,
              panel_gap   = panel_gap,
              align_mode  = align_mode,
              axis_mode   = axis_mode
            )
            attr(recomposed, "gsea_running_score_plotlist") <- running_parts[[j]]
            attr(recomposed, "gsea_running_score_rel_heights") <- rel_heights
            attr(recomposed, "gsea_running_score_panel_gap") <- panel_gap
            class(recomposed) <- unique(c("gsea_running_score_alignable", class(recomposed)))
            recomposed
          })
          cowplot::align_plots(
            plotlist = recomposed_subset,
            align    = align_mode,
            axis     = axis_mode
          )
        } else {
          cowplot::align_plots(
            plotlist = subset,
            align    = align_mode,
            axis     = axis_mode
          )
        }
      }, error = function(e) {
        debug_log(paste("Alignment failed for group", grp, ":", e$message), 1)
        subset
      })
      if (length(aligned_subset) == length(subset)) {
        for (j in seq_along(aligned_subset)) {
          marker_classes <- intersect(
            class(subset[[j]]),
            c("gsea_enrichment_map_alignable", "go_enrichment_map_alignable")
          )
          if (length(marker_classes) > 0) {
            class(aligned_subset[[j]]) <- unique(c(marker_classes, class(aligned_subset[[j]])))
            aligned_subset[[j]] <- .make_enrichment_map_gtable_alignable(
              aligned_subset[[j]],
              nm = if (!is.null(names(subset)) && length(names(subset)) >= j) names(subset)[j] else "",
              debug_log = debug_log
            )
          }
        }
      }
      aligned_candidates[idx] <- aligned_subset
    }
  }

  aligned               <- plots
  aligned[!is_composite] <- aligned_candidates
  if (!is.null(names(plots)) && length(names(plots)) == length(aligned))
    names(aligned) <- names(plots)
  aligned
}

# ---------------------------------------------------------------------------
# 4. Span-layout primitives
# ---------------------------------------------------------------------------

# Calculate normalised (0-1) cowplot canvas coordinates for each plot given
# its assignment (row, col, rowspan, colspan) in the grid.
compute_span_coordinates <- function(assignments, nrow, ncol, gap = 0,
                                     debug_log = function(...) invisible(NULL)) {
  debug_log("Computing span coordinates", 2)
  gap    <- max(0, min(0.1, suppressWarnings(as.numeric(gap %||% 0))))
  cell_w <- 1 / max(1, ncol)
  cell_h <- 1 / max(1, nrow)
  coords <- list()
  for (nm in names(assignments)) {
    a       <- assignments[[nm]]
    x_left  <- (a$col - 1) * cell_w
    y_bottom <- 1 - ((a$row + a$rowspan - 1) * cell_h)
    width   <- a$colspan * cell_w
    height  <- a$rowspan * cell_h
    x_left  <- x_left  + gap / 2
    y_bottom <- y_bottom + gap / 2
    width   <- max(0, width  - gap)
    height  <- max(0, height - gap)
    debug_log(
      sprintf("Coords '%s' -> x=%.3f y=%.3f w=%.3f h=%.3f",
              nm, x_left, y_bottom, width, height),
      2
    )
    coords[[nm]] <- list(x = x_left, y = y_bottom,
                         width = width, height = height)
  }
  coords
}

# Place plots into the grid sequentially (left-to-right, top-to-bottom)
# without backfilling.  Returns a list with $assignments, $matrix,
# $rel_heights, and $rel_widths.
create_span_layout_matrix_fixed <- function(plots, plot_names, plot_spans,
                                             nrow, ncol,
                                             debug_log = function(...) invisible(NULL)) {
  if (length(plot_names) == 0) {
    debug_log("No plot names provided", 1)
    return(NULL)
  }
  debug_log(
    paste("Starting span layout for", length(plot_names), "plots"), 2
  )

  result <- tryCatch({
    layout_matrix  <- matrix(0L, nrow = nrow, ncol = ncol)
    plot_assignments <- list()
    index_counter  <- 1L
    cur_r          <- 1L
    cur_c          <- 1L

    place_block <- function(r0, c0, rs, cs, idx) {
      layout_matrix[r0:(r0 + rs - 1L), c0:(c0 + cs - 1L)] <<- idx
    }

    for (i in seq_along(plot_names)) {
      nm <- plot_names[i]
      sp <- plot_spans[[nm]] %||% list(colspan = 1L, rowspan = 1L)
      cs <- max(1L, min(as.integer(sp$colspan %||% 1L), ncol))
      rs <- max(1L, as.integer(sp$rowspan %||% 1L))

      placed  <- FALSE
      r       <- cur_r
      c_start <- cur_c

      repeat {
        if (r + rs - 1L > nrow) break  # no space left in visible grid

        for (c in c_start:ncol) {
          end_r <- r + rs - 1L
          end_c <- c + cs - 1L
          if (end_c > ncol) break
          block <- layout_matrix[r:end_r, c:end_c, drop = FALSE]
          if (any(block != 0L)) next
          # Place this block.
          place_block(r, c, rs, cs, index_counter)
          plot_assignments[[nm]] <- list(
            index   = index_counter,
            row     = r,
            col     = c,
            rowspan = rs,
            colspan = cs
          )
          # Advance cursor without backfilling.
          cur_r <- r
          cur_c <- end_c + 1L
          if (cur_c > ncol) { cur_r <- cur_r + 1L; cur_c <- 1L }
          index_counter <- index_counter + 1L
          placed        <- TRUE
          break
        }
        if (placed) break
        r       <- r + 1L
        c_start <- 1L
      }

      if (!placed)
        debug_log(paste("No space for", nm, "- skipping"), 1)
    }

    for (r in 1:nrow)
      debug_log(
        paste("Matrix row", r, ":", paste(layout_matrix[r, ], collapse = " ")),
        2
      )

    idx_to_name <- setNames(
      names(plot_assignments),
      vapply(plot_assignments, function(a) a$index, integer(1))
    )

    plot_list <- vector("list", nrow * ncol)
    position  <- 1L
    for (r in 1:nrow) {
      for (c in 1:ncol) {
        val <- layout_matrix[r, c]
        if (val == 0L) {
          plot_list[[position]] <- create_empty_plot()
        } else {
          nm <- idx_to_name[[as.character(val)]]
          a  <- plot_assignments[[nm]]
          plot_list[[position]] <- if (!is.null(a) && r == a$row && c == a$col)
            plots[[nm]] else create_empty_plot()
        }
        position <- position + 1L
      }
    }

    rel_heights <- rep(1, nrow)
    rel_widths  <- rep(1, ncol)
    for (nm in names(plot_assignments)) {
      a <- plot_assignments[[nm]]
      if (a$rowspan > 1)
        for (r in a$row:(a$row + a$rowspan - 1))
          if (r <= nrow) rel_heights[r] <- rel_heights[r] * 1.2
      if (a$colspan > 1)
        for (c in a$col:(a$col + a$colspan - 1))
          if (c <= ncol) rel_widths[c] <- rel_widths[c] * 1.2
    }

    list(matrix = layout_matrix, plot_list = plot_list,
         rel_heights = rel_heights, rel_widths = rel_widths,
         assignments = plot_assignments)
  }, error = function(e) {
    debug_log(paste("Error in span layout matrix creation:", e$message), 1)
    NULL
  })

  debug_log("Span layout matrix creation completed", 2)
  result
}

# ---------------------------------------------------------------------------
# 5. Composition entry points
# ---------------------------------------------------------------------------

# Canvas-based span composition using cowplot::ggdraw().
# Returns a composed ggplot or NULL if composition is not applicable.
compose_grid_with_spans_safe <- function(plots, settings, labels = NULL,
                                          rv = NULL,
                                          debug_log = function(...) invisible(NULL)) {
  if (length(plots) == 0) {
    debug_log("No plots provided to span composition", 1)
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  plot_spans <- settings$plot_spans
  if (is.null(plot_spans) || length(plot_spans) == 0) {
    debug_log("No plot spans found", 2)
    return(NULL)
  }
  debug_log("Creating span grid with canvas placement", 1)

  plot_names <- names(plots)
  if (is.null(plot_names) || length(plot_names) == 0) {
    plot_names <- names(plot_spans)
    if (is.null(plot_names) || length(plot_names) == 0) {
      plot_names <- paste0("plot_", seq_along(plots))
      names(plots) <- plot_names
    }
  }

  valid_spans <- plot_spans[names(plot_spans) %in% plot_names]
  if (length(valid_spans) == 0) return(NULL)

  nrow <- settings$nrow
  ncol <- settings$ncol

  layout_result <- create_span_layout_matrix_fixed(
    plots, plot_names, valid_spans, nrow, ncol, debug_log
  )
  if (is.null(layout_result) || length(layout_result$assignments) == 0)
    return(NULL)

  if (is.null(labels)) {
    labels <- tryCatch(
      build_labels(settings, plot_names, debug_log = debug_log),
      error = function(e) {
        debug_log(paste("build_labels failed:", e$message), 1)
        NULL
      }
    )
  }
  if (!is.null(labels) && is.null(names(labels))) names(labels) <- plot_names

  gap    <- tryCatch(as.numeric(settings$cell_gap %||% 0), error = function(e) 0)
  coords <- compute_span_coordinates(layout_result$assignments, nrow, ncol,
                                      gap = gap, debug_log = debug_log)

  result <- tryCatch({
    p     <- cowplot::ggdraw()
    plots <- align_plots_if_requested(plots, settings, rv = rv,
                                      debug_log = debug_log)
    plots <- apply_per_plot_margins(plots, settings, debug_log = debug_log)

    for (nm in plot_names) {
      a <- coords[[nm]]
      if (is.null(a)) next
      p <- p + cowplot::draw_plot(plots[[nm]], x = a$x, y = a$y,
                                   width = a$width, height = a$height)
      if (!is.null(labels)) {
        lbl <- labels[[nm]]
        if (!is.null(lbl) && nzchar(lbl)) {
          lx   <- a$x + 0.01 * a$width
          ly   <- a$y + a$height - 0.01 * a$height
          size <- tryCatch(as.numeric(settings$label_size %||% 12),
                           error = function(e) 12)
          p <- p + cowplot::draw_plot_label(lbl, x = lx, y = ly,
                                             hjust = 0, vjust = 1,
                                             size = size)
        }
      }
    }
    p
  }, error = function(e) {
    debug_log(paste("Canvas-based span composition error:", e$message), 1)
    NULL
  })

  if (!is.null(result))
    debug_log("Span grid created successfully (canvas-based)", 1)
  result
}

# Main entry point: assemble plots into a grid using cowplot::plot_grid.
# Uses span-based canvas layout when any plot has a non-default span.
# rv is optional; pass it from the server closure for source-based features.
compose_grid <- function(plots, settings, labels = NULL, force_regular = FALSE,
                          rv = NULL,
                          debug_log = function(...) invisible(NULL)) {
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "Package 'cowplot' not installed", size = 6))
  }

  has_custom_spans <- !is.null(settings$plot_spans) &&
    length(settings$plot_spans) > 0 &&
    any(vapply(settings$plot_spans, function(sp) {
      (as.integer(sp$colspan %||% 1L)) > 1L ||
        (as.integer(sp$rowspan %||% 1L)) > 1L
    }, logical(1)))

  if (!force_regular && has_custom_spans) {
    debug_log("Attempting span-based grid composition", 2)
    span_result <- tryCatch(
      compose_grid_with_spans_safe(plots, settings, labels, rv = rv,
                                    debug_log = debug_log),
      error = function(e) {
        debug_log(paste("Span composition failed:", e$message), 1)
        NULL
      }
    )
    if (!is.null(span_result)) return(span_result)
    debug_log("Span composition failed; falling back to regular grid", 1)
  }

  if (is.null(labels)) {
    labels <- tryCatch(
      build_labels(settings, names(plots), debug_log = debug_log),
      error = function(e) {
        debug_log(paste("build_labels failed:", e$message), 1)
        NULL
      }
    )
  }

  margins <- settings$margins %||% list(top = 10, right = 10, bottom = 10, left = 10)

  base_rel_heights <- rep(1, settings$nrow)
  base_rel_widths  <- rep(1, settings$ncol)
  if (margins$top > 10 || margins$bottom > 10) {
    mf <- max(margins$top, margins$bottom) / 10
    base_rel_heights <- base_rel_heights * mf
  }
  if (margins$left > 10 || margins$right > 10) {
    mf <- max(margins$left, margins$right) / 10
    base_rel_widths <- base_rel_widths * mf
  }

  align_mode  <- settings$align %||% "none"
  nrow        <- settings$nrow
  ncol        <- settings$ncol
  total_plots <- length(plots)

  # Downgrade hv to h/v for degenerate single-row or single-column grids.
  final_align <- if (align_mode == "hv") {
    if (nrow == 1 && ncol > 1) {
      debug_log("Switching from 'hv' to 'h' for single-row layout", 1)
      "h"
    } else if (ncol == 1 && nrow > 1) {
      debug_log("Switching from 'hv' to 'v' for single-column layout", 1)
      "v"
    } else align_mode
  } else align_mode

  legend_position <- settings$force_legend_position %||% "preserve"
  debug_log(paste("Legend control mode:", legend_position), 2)

  if (length(plots) == 0) {
    debug_log("No plots provided to compose_grid", 1)
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "No plots in grid", size = 6))
  }

  # Build source lookup from rv when available.
  source_lookup <- list()
  if (!is.null(rv) && !is.null(rv$gridplot_selection)) {
    for (plot_id in names(plots)) {
      entry <- rv$gridplot_selection[[plot_id]]
      if (!is.null(entry$source))
        source_lookup[[plot_id]] <- entry$source
    }
  }

  # Apply legend control to each plot.
  legend_successes <- 0L
  legend_failures  <- 0L
  prepared_plots <- lapply(seq_along(plots), function(i) {
    plot_obj  <- plots[[i]]
    plot_name <- if (!is.null(names(plots))) names(plots)[i] else paste0("plot_", i)
    source_info <- source_lookup[[plot_name]]

    debug_log(
      paste("Processing plot", i, ":", plot_name, "| source:", source_info), 2
    )
    if (is.null(plot_obj)) {
      debug_log(paste("Plot", plot_name, "is NULL"), 1)
      return(plot_obj)
    }

    if (legend_position == "preserve") return(plot_obj)

    result <- force_legend_maximum_aggression(
      plot_obj, legend_position, plot_name, source_info, debug_log
    )
    test_res <- test_legend_control_effectiveness(result, legend_position)
    if (test_res$works) {
      legend_successes <<- legend_successes + 1L
    } else {
      legend_failures  <<- legend_failures  + 1L
    }
    result
  })
  names(prepared_plots) <- names(plots)

  if (legend_position != "preserve" && (legend_successes + legend_failures) > 0)
    debug_log(
      paste("Legend control: success =", legend_successes,
            "| failure =", legend_failures),
      2
    )

  if (final_align %in% c("h", "v", "hv")) {
    spacing_factor   <- 1.2
    base_rel_widths  <- base_rel_widths  * spacing_factor
    base_rel_heights <- base_rel_heights * spacing_factor
  }

  aligned_plots <- if (final_align != "none") {
    align_plots_if_requested(prepared_plots, settings, rv = rv,
                             debug_log = debug_log)
  } else {
    prepared_plots
  }

  # Per-plot margin offsets must be applied after all alignment has finished,
  # so user offsets start from the aligned plot geometry instead of changing
  # the alignment calculation itself. apply_per_plot_margins() also supports
  # aligned gtables returned by cowplot::align_plots().
  aligned_plots <- apply_per_plot_margins(aligned_plots, settings,
                                          debug_log = debug_log)

  args <- list(
    plotlist    = aligned_plots,
    nrow        = nrow,
    ncol        = ncol,
    align       = "none",
    rel_heights = base_rel_heights,
    rel_widths  = base_rel_widths
  )
  if (!is.null(labels))             args$labels     <- unname(labels)
  if (!is.null(settings$label_size)) args$label_size <- as.numeric(settings$label_size)

  result <- tryCatch(
    do.call(cowplot::plot_grid, args),
    error = function(e) {
      debug_log(paste("Grid composition error:", e$message), 1)
      if (final_align != "none") {
        debug_log("Retrying without alignment", 1)
        args$align <- "none"
        tryCatch(
          do.call(cowplot::plot_grid, args),
          error = function(e2) {
            debug_log(paste("Fallback composition also failed:", e2$message), 1)
            ggplot2::ggplot() + ggplot2::theme_void() +
              ggplot2::annotate("text", x = 0.5, y = 0.5,
                                label = paste0("Grid error: ", e$message),
                                size = 5)
          }
        )
      } else {
        ggplot2::ggplot() + ggplot2::theme_void() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste0("Grid error: ", e$message),
                            size = 5)
      }
    }
  )

  debug_log("Grid composition completed", 2)
  result
}

# ---------------------------------------------------------------------------
# Fallback
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

if (!exists("debug_log", inherits = FALSE)) {
  debug_log <- function(...) invisible(NULL)
}
