# modules/Grid/Grid_legend.R
#
# Purpose:
#   Provides legend detection, plot-type identification, and legend control
#   for the Grid module.  These utilities are called exclusively from
#   Grid_composition.R during grid assembly.
#
# Architecture:
#   Pure utility layer with no Shiny reactivity.  Loaded into modEnv by
#   Grid_module.R and used internally by compose_grid() in Grid_composition.R.
#   No server logic lives here.
#
# Structure:
#   1. Legend detection helpers (has_legend, has_visible_legend_comprehensive)
#   2. Source-based plot-type detection
#      (detect_plot_type_by_source, analyze_converted_plot_structure)
#   3. Legend control
#      (test_legend_control_effectiveness, force_legend_maximum_aggression)
#
# Future developers:
#   - force_legend_maximum_aggression is the main entry point; the detection
#     helpers are its internal building blocks.
#   - All public functions accept debug_log as an explicit parameter with a
#     silent no-op default.
#   - Do not add server or reactive logic here.

# ---------------------------------------------------------------------------
# 1. Legend detection helpers
# ---------------------------------------------------------------------------

# Quick check for a visible legend using ggplot_build then a heuristic fallback.
# This is a lightweight but less complete check.  Use
# has_visible_legend_comprehensive() when you also need to inspect theme and
# scale state (e.g. when a legend could be suppressed via legend.position or
# through explicit guide removal).  Both functions are utility helpers kept for
# developer use; they are not called by the composition pipeline itself.
has_legend <- function(plot, debug_log = function(...) invisible(NULL)) {
  if (!inherits(plot, "ggplot")) return(FALSE)
  tryCatch({
    built_plot  <- ggplot2::ggplot_build(plot)
    plot_table  <- ggplot2::ggplot_gtable(built_plot)
    has_legends <- length(grep("guide", plot_table$layout$name)) > 0
    debug_log(paste("Plot has legend:", has_legends), 2)
    has_legends
  }, error = function(e) {
    tryCatch({
      mappings         <- plot$mapping
      legend_aes       <- c("colour", "color", "fill", "shape", "size",
                             "alpha", "linetype")
      layers_have_aes  <- any(sapply(plot$layers, function(layer) {
        any(names(layer$mapping) %in% legend_aes)
      }))
      main_has_legend  <- any(names(mappings) %in% legend_aes)
      result           <- layers_have_aes || main_has_legend
      debug_log(paste("Plot has legend (fallback method):", result), 2)
      result
    }, error = function(e2) {
      debug_log(paste("Could not determine if plot has legend:", e2$message), 1)
      TRUE  # conservative: assume a legend might be present
    })
  })
}

# Comprehensive check combining theme inspection and aesthetic/scale analysis.
has_visible_legend_comprehensive <- function(plot,
                                             debug_log = function(...) invisible(NULL)) {
  if (!inherits(plot, "ggplot")) {
    debug_log("has_visible_legend_comprehensive: not a ggplot object", 2)
    return(FALSE)
  }
  tryCatch({
    legend_position <- plot$theme$legend.position
    if (!is.null(legend_position) && identical(legend_position, "none")) {
      debug_log("Legend explicitly hidden via legend.position = 'none'", 2)
      return(FALSE)
    }

    legend_aesthetics <- c("colour", "color", "fill", "shape", "size",
                           "alpha", "linetype")
    has_main_legend_aes  <- any(names(plot$mapping) %in% legend_aesthetics)
    layer_legend_aes     <- FALSE
    for (layer in plot$layers) {
      if (any(names(layer$mapping) %in% legend_aesthetics)) {
        layer_legend_aes <- TRUE
        break
      }
    }
    has_legend_aesthetics <- has_main_legend_aes || layer_legend_aes

    has_manual_scales <- FALSE
    if (!is.null(plot$scales) && length(plot$scales$scales) > 0) {
      for (scale in plot$scales$scales) {
        if (!is.null(scale$aesthetics) &&
            any(scale$aesthetics %in% legend_aesthetics)) {
          has_manual_scales <- TRUE
          break
        }
      }
    }

    is_not_hidden <- is.null(legend_position) ||
      !identical(legend_position, "none")
    result <- (has_legend_aesthetics || has_manual_scales) && is_not_hidden

    debug_log(
      paste("Legend detection:",
            "aesthetics =", has_legend_aesthetics,
            "| manual scales =", has_manual_scales,
            "| not hidden =", is_not_hidden,
            "| result =", result),
      2
    )
    result
  }, error = function(e) {
    debug_log(paste("Legend detection error:", e$message), 1)
    FALSE
  })
}

# ---------------------------------------------------------------------------
# 2. Source-based plot-type detection
# ---------------------------------------------------------------------------

# Classify a plot using its source module, name, and structure.  Returns a
# list with fields $type, $needs_special_handling, and $reason.
detect_plot_type_by_source <- function(plot_obj, plot_name = "unknown",
                                       source_info = NULL,
                                       debug_log = function(...) invisible(NULL)) {
  debug_log(
    paste("Analysing plot by source - Name:", plot_name,
          "| Source:", source_info),
    2
  )

  if (is.null(plot_obj))
    return(list(type = "null", needs_special_handling = FALSE))

  if (any(class(plot_obj) %in% c("Heatmap", "HeatmapList")))
    return(list(type = "complexheatmap_object", needs_special_handling = TRUE,
                reason = "True ComplexHeatmap object detected by class"))

  if (!inherits(plot_obj, "ggplot"))
    return(list(type = "non_ggplot", needs_special_handling = FALSE,
                reason = paste("Not ggplot - class:",
                               paste(class(plot_obj), collapse = ", "))))

  # Source module heuristics
  if (!is.null(source_info)) {
    if (grepl("heatmap", tolower(source_info)))
      return(list(type = "heatmap_module_plot", needs_special_handling = TRUE,
                  reason = "Plot from Heatmap module - likely converted ComplexHeatmap"))

    if (grepl("gsea", tolower(source_info))) {
      if (grepl("running.*score|score.*plot", tolower(plot_name)))
        return(list(type = "gsea_running_score", needs_special_handling = TRUE,
                    reason = "GSEA Running Score plot - pre-combined cowplot structure"))
      if (grepl("pubmed|citation", tolower(plot_name)))
        return(list(type = "gsea_pubmed_citation", needs_special_handling = TRUE,
                    reason = "GSEA PubMed Citation plot - pre-combined cowplot structure"))
      return(list(type = "gsea_other_plot", needs_special_handling = FALSE,
                  reason = "Other GSEA plot - should work with standard theme()"))
    }

    if (grepl("go", tolower(source_info))) {
      if (grepl("pubmed|citation", tolower(plot_name)))
        return(list(type = "go_pubmed_citation", needs_special_handling = TRUE,
                    reason = "GO PubMed Citation plot - pre-combined cowplot structure"))
    }
  }

  # Plot-name heuristics
  plot_name_lower <- tolower(plot_name)
  if (grepl("heatmap", plot_name_lower))
    return(list(type = "heatmap_by_name", needs_special_handling = TRUE,
                reason = "Heatmap detected in plot name"))
  if (grepl("running.*score|score.*running", plot_name_lower))
    return(list(type = "running_score_by_name", needs_special_handling = TRUE,
                reason = "Running score plot detected in name"))
  if (grepl("pubmed|citation", plot_name_lower))
    return(list(type = "pubmed_by_name", needs_special_handling = TRUE,
                reason = "PubMed/citation plot detected in name"))

  # Deep structure analysis
  structure_info <- analyze_converted_plot_structure(plot_obj, debug_log)
  if (structure_info$is_complex)
    return(list(type = structure_info$detected_type,
                needs_special_handling = TRUE,
                reason = structure_info$reason))

  list(type = "ggplot_simple", needs_special_handling = FALSE,
       reason = "Standard ggplot structure")
}

# Inspect plot data and layers for patterns that suggest a complex or
# converted plot (enrichplot, heatmap-derived, multi-layer, faceted).
analyze_converted_plot_structure <- function(plot_obj,
                                             debug_log = function(...) invisible(NULL)) {
  tryCatch({
    if (!is.null(plot_obj$data) && is.data.frame(plot_obj$data)) {
      data_cols <- names(plot_obj$data)
      enrichplot_cols <- c("x", "y", "runningScore", "position", "gene",
                           "pvalue", "NES")
      if (length(intersect(data_cols, enrichplot_cols)) >= 2)
        return(list(is_complex = TRUE,
                    detected_type = "enrichplot_derived",
                    reason = "Enrichplot-derived structure detected in data columns"))

      if (any(c("value", "Var1", "Var2", "x", "y", "fill") %in% data_cols) &&
          nrow(plot_obj$data) > 100)
        return(list(is_complex = TRUE,
                    detected_type = "heatmap_derived",
                    reason = paste("Heatmap-like structure -",
                                   nrow(plot_obj$data), "data points")))
    }

    if (!is.null(plot_obj$layers) && length(plot_obj$layers) > 5) {
      geom_types <- sapply(plot_obj$layers, function(layer) {
        if (!is.null(layer$geom)) class(layer$geom)[1] else "unknown"
      })
      return(list(is_complex = TRUE,
                  detected_type = "multilayer_complex",
                  reason = paste("Complex multi-layer plot -",
                                 length(plot_obj$layers), "layers:",
                                 paste(unique(geom_types), collapse = ", "))))
    }

    if (!is.null(plot_obj$facet) &&
        !inherits(plot_obj$facet, "FacetNull"))
      return(list(is_complex = TRUE,
                  detected_type = "faceted_complex",
                  reason = paste("Faceted plot -",
                                 class(plot_obj$facet)[1])))

    list(is_complex = FALSE, detected_type = "simple",
         reason = "No complex patterns detected")
  }, error = function(e) {
    debug_log(paste("Error in deep structure analysis:", e$message), 2)
    list(is_complex = FALSE, detected_type = "analysis_failed",
         reason = paste("Analysis failed:", e$message))
  })
}

# ---------------------------------------------------------------------------
# 3. Legend control
# ---------------------------------------------------------------------------

# Test whether applying theme() successfully changes the legend position.
# Returns list(works = logical, reason = character).
test_legend_control_effectiveness <- function(plot_obj,
                                              target_position = "bottom") {
  if (!inherits(plot_obj, "ggplot"))
    return(list(works = FALSE, reason = "Not a ggplot object"))
  tryCatch({
    original_pos  <- plot_obj$theme$legend.position
    modified_plot <- if (target_position == "none") {
      plot_obj + ggplot2::theme(legend.position = "none")
    } else {
      plot_obj + ggplot2::theme(legend.position   = target_position,
                                legend.justification = "center")
    }
    new_pos <- modified_plot$theme$legend.position
    if (is.null(new_pos))
      return(list(works = FALSE,
                  reason = "Theme modification failed - new position is NULL"))
    expected <- (target_position == "none" && identical(new_pos, "none")) ||
      (target_position != "none" && identical(new_pos, target_position))
    if (expected) {
      list(works = TRUE,
           reason = paste("Changed from",
                          if (is.null(original_pos)) "NULL" else original_pos,
                          "to", new_pos))
    } else {
      list(works = FALSE,
           reason = paste("Position not changed as expected - got", new_pos,
                          "instead of", target_position))
    }
  }, error = function(e) {
    list(works = FALSE,
         reason = paste("Error during theme test:", e$message))
  })
}

# Apply legend control with progressive fallbacks.  Returns the modified plot.
# Tries: standard theme(), double theme(), force guides(), with final
# fallback of a warning annotation if all approaches fail.
force_legend_maximum_aggression <- function(plot_obj, legend_position,
                                            plot_name   = "unknown",
                                            source_info = NULL,
                                            debug_log   = function(...) invisible(NULL)) {
  if (legend_position == "preserve") return(plot_obj)

  test_result <- test_legend_control_effectiveness(plot_obj, legend_position)
  if (test_result$works) {
    debug_log(
      paste("Standard theme() works for", plot_name, "-", test_result$reason), 2
    )
    return(if (legend_position == "none") {
      plot_obj + ggplot2::theme(legend.position = "none")
    } else {
      plot_obj + ggplot2::theme(legend.position      = legend_position,
                                legend.justification = "center")
    })
  }

  debug_log(
    paste("Standard theme() failed for", plot_name, "-", test_result$reason), 2
  )
  plot_info <- detect_plot_type_by_source(plot_obj, plot_name, source_info,
                                          debug_log)
  debug_log(
    paste("Detected type:", plot_info$type,
          "| special handling:", plot_info$needs_special_handling),
    2
  )

  approaches <- list(
    double_theme = function(p) {
      p + ggplot2::theme(legend.position = legend_position) +
        ggplot2::theme(legend.position      = legend_position,
                       legend.justification = "center")
    },
    force_guides = function(p) {
      if (legend_position == "none") {
        p + ggplot2::guides(
          colour = "none", color = "none", fill  = "none",
          shape  = "none", size  = "none", alpha = "none"
        )
      } else {
        p + ggplot2::theme(legend.position      = legend_position,
                           legend.justification = "center")
      }
    }
  )

  for (approach_name in names(approaches)) {
    result <- tryCatch(approaches[[approach_name]](plot_obj),
                       error = function(e) {
                         debug_log(paste("Approach", approach_name, "failed:",
                                         e$message), 2)
                         NULL
                       })
    if (!is.null(result)) {
      test2 <- test_legend_control_effectiveness(result, legend_position)
      if (test2$works) {
        debug_log(paste("Approach", approach_name, "succeeded for", plot_name), 1)
        return(result)
      }
    }
  }

  debug_log(
    paste("All legend control approaches failed for", plot_name,
          "- adding warning annotation"),
    1
  )
  tryCatch(
    plot_obj + ggplot2::annotate(
      "text", x = Inf, y = Inf,
      label    = paste("Legend position control failed\nTarget:", legend_position),
      hjust    = 1.1, vjust = 1.1,
      size     = 2.5, color = "red", alpha = 0.8, fontface = "italic"
    ),
    error = function(e) {
      debug_log(paste("Warning annotation also failed:", e$message), 1)
      plot_obj
    }
  )
}

# ---------------------------------------------------------------------------
# Fallback
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

if (!exists("debug_log", inherits = FALSE)) {
  debug_log <- function(...) invisible(NULL)
}
