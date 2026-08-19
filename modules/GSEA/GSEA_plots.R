# GSEA_plots.R
# Enhanced plot rendering functions for GSEA module
# Handles multiple plot types with proper pathway selection logic

# ========================================
# Helper Functions for Plot Rendering
# ========================================

#' Render GSEA plot to output
render_gsea_plot <- function(output, plot_obj, height = 600, message = NULL, current_plot = NULL) {
  # Calculate responsive dimensions
  actual_height <- max(height, 300)

  # Store the plot for downloads if current_plot reactive is provided
  if (!is.null(current_plot) && is.function(current_plot)) {
    current_plot(plot_obj)
  }

  # Create UI container
  output[["GSEAplot_container"]] <- renderUI({
    tagList(
      if (!is.null(message)) {
        div(
          style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; padding: 10px; margin-bottom: 10px; color: #155724;",
          message
        )
      },
      plotOutput("GSEAplot_custom",
                 height = paste0(actual_height, "px"),
                 width = "100%")
    )
  })

  # Render the actual plot
  output[["GSEAplot_custom"]] <- renderPlot({
    if (!is.null(plot_obj)) {
      print(plot_obj)
    }
  }, height = actual_height)
}

#' Render notification message
render_gsea_notification <- function(output, message, height = 600) {
  output$GSEAplot_container <- renderUI({
    div(
      style = paste0("height: ", height, "px; display: flex; align-items: center; justify-content: center; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;"),
      div(
        style = "text-align: center; color: #6c757d;",
        h4(message, style = "margin-bottom: 10px;"),
        p("Adjust your selection criteria and try again.")
      )
    )
  })
}

# ========================================
# Pathway Index Helper Functions
# ========================================

#' Get single pathway index (first selected)
get_pathway_index <- function(gsea_results, selected_pathways, single = TRUE) {
  if (is.null(selected_pathways) || length(selected_pathways) == 0) {
    return(1)  # Default to first pathway
  }

  results_df <- as.data.frame(gsea_results$Results)
  pathway_match <- which(results_df$Description %in% selected_pathways)

  if (length(pathway_match) > 0) {
    return(if (single) pathway_match[1] else pathway_match)
  } else {
    return(1)  # Fallback to first pathway
  }
}

#' Get multiple pathway indices - DEPRECATED for running score plots
get_pathway_indices <- function(gsea_results, selected_pathways) {
  if (is.null(selected_pathways) || length(selected_pathways) == 0) {
    return(1:min(5, nrow(as.data.frame(gsea_results$Results))))  # Default to first 5
  }

  results_df <- as.data.frame(gsea_results$Results)
  pathway_matches <- which(results_df$Description %in% selected_pathways)

  return(if (length(pathway_matches) > 0) pathway_matches else c(1))
}

# ========================================
# Legend Positioning Helpers
# ========================================

assemble_plot_with_legend <- function(main_plot,
                                      legend_grob,
                                      legend_position = "right",
                                      legend_height_frac = 0.18,
                                      legend_width_frac = 0.25) {
  require(cowplot)
  if (is.null(legend_grob) || identical(legend_position, "none")) return(main_plot)

  if (legend_position == "top") {
    legend_centered <- cowplot::ggdraw() +
      cowplot::draw_grob(legend_grob, x = 0.5, y = 0.5, halign = 0.5, valign = 0.5)
    cowplot::plot_grid(legend_centered, main_plot, ncol = 1,
                       rel_heights = c(legend_height_frac, 1))
  } else if (legend_position == "bottom") {
    legend_centered <- cowplot::ggdraw() +
      cowplot::draw_grob(legend_grob, x = 0.5, y = 0.5, halign = 0.5, valign = 0.5)
    cowplot::plot_grid(main_plot, legend_centered, ncol = 1,
                       rel_heights = c(1, legend_height_frac))
  } else if (legend_position == "left") {
    cowplot::plot_grid(legend_grob, main_plot, nrow = 1,
                       rel_widths = c(legend_width_frac, 1),
                       align = "h", axis = "t")
  } else { # right
    cowplot::plot_grid(main_plot, legend_grob, nrow = 1,
                       rel_widths = c(1, legend_width_frac),
                       align = "h", axis = "t")
  }
}

build_horizontal_colorbar <- function(title, reverse = TRUE, rotate_labels = TRUE, order = 2) {
  guide_colorbar(
    order          = order,
    direction      = "horizontal",
    title.position = "top",
    title.hjust    = 0.5,
    label.position = "bottom",
    reverse        = reverse,
    barwidth       = unit(6, "cm"),
    barheight      = unit(0.5, "cm"),
    label.theme    = if (rotate_labels) element_text(angle = 45, hjust = 0.5, vjust = 0.5) else element_text()
  )
}

build_horizontal_size_legend <- function(title, rotate_labels = TRUE, order = 1) {
  guide_legend(
    order          = order,
    direction      = "horizontal",
    title.position = "top",
    title.hjust    = 0.5,
    label.position = "bottom",
    label.theme    = if (rotate_labels) element_text(angle = 45, hjust = 0.5, vjust = 0.5) else element_text(),
    override.aes   = list(color = "grey40", alpha = 0.85)
  )
}

build_vertical_colorbar <- function(reverse = TRUE, order = 2) {
  guide_colorbar(
    order     = order,
    direction = "vertical",
    reverse   = reverse
  )
}

build_vertical_size_legend <- function(order = 1) {
  guide_legend(
    order     = order,
    direction = "vertical",
    override.aes = list(color = "grey40", alpha = 0.85)
  )
}

gsea_size_guide <- function(horizontal, order = 1) {
  if (horizontal) {
    guide_legend(
      order = order,
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      label.position = if (.ggp_has_label_position) "bottom" else "right",
      override.aes = list(color = "grey40", alpha = 0.85)
    )
  } else {
    guide_legend(
      order = order,
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(color = "grey40", alpha = 0.85)
    )
  }
}

gsea_colorbar_guide <- function(horizontal, reverse = FALSE, order = 2) {
  if (horizontal) {
    guide_colorbar(
      order = order,
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      label.position = if (.ggp_has_label_position) "bottom" else "right",
      reverse = reverse,
      barwidth = unit(6, "cm"),
      barheight = unit(0.5, "cm")
    )
  } else {
    guide_colorbar(
      order = order,
      direction = "vertical",
      reverse = reverse
    )
  }
}

apply_gsea_legend_layout <- function(p, legend_position, sizes, rotate_when_horizontal = TRUE) {
  if (legend_position == "none") return(p + theme(legend.position = "none"))
  horizontal <- legend_position %in% c("top","bottom")
  p + theme(
    legend.position     = legend_position,
    legend.direction    = if (horizontal) "horizontal" else "vertical",
    legend.box          = if (horizontal) "horizontal" else "vertical",
    legend.box.just     = "center",
    legend.justification= "center",
    legend.spacing.x    = unit(6, "pt"),
    legend.key.height   = unit(12, "pt"),
    legend.text         = element_text(
      size  = sizes$legendText,
      angle = if (horizontal && rotate_when_horizontal) 45 else 0,
      hjust = 0.5, vjust = 0.5
    ),
    legend.title        = element_text(size = sizes$legendTitle)
  )
}

.ggp_has_label_position <- local({
  v <- tryCatch(utils::packageVersion("ggplot2"), error = function(e) NULL)
  if (is.null(v)) FALSE else v >= "3.5.0"
})

#' Build size guide for GSEA plots - FINAL with proper horizontal layout
.gsea_build_size_guide <- function(horizontal, order = 1) {
  if (horizontal) {
    guide_legend(
      order = order,
      direction = "horizontal",
      title.position = "top",        # Title above legend elements
      title.hjust = 0.5,             # Center title horizontally
      label.position = "bottom",     # Labels below symbols
      nrow = 2,                      # MULTI-ROW layout like dotplot
      byrow = TRUE,                  # Fill by row
      override.aes = list(color = "grey40", alpha = 0.85)
    )
  } else {
    guide_legend(
      order = order,
      direction = "vertical",
      title.position = "top",        # Title ABOVE elements (not left)
      title.hjust = 0,               # LEFT-ALIGNED title
      override.aes = list(color = "grey40", alpha = 0.85)
    )
  }
}

#' Build colorbar guide for GSEA plots - FINAL with proper horizontal layout
.gsea_build_colorbar_guide <- function(horizontal, reverse = FALSE, order = 2) {
  if (horizontal) {
    guide_colorbar(
      order = order,
      direction = "horizontal",
      title.position = "top",        # Title above colorbar
      title.hjust = 0.5,             # Center title horizontally
      label.position = "bottom",     # Labels below colorbar
      barwidth = unit(8, "cm"),      # WIDER bar for better spacing
      barheight = unit(0.8, "cm"),   # TALLER bar for better spacing
      reverse = reverse
    )
  } else {
    guide_colorbar(
      order = order,
      direction = "vertical",
      title.position = "top",        # Title ABOVE colorbar (not left)
      title.hjust = 0,               # LEFT-ALIGNED title
      reverse = reverse
    )
  }
}


.gsea_make_size_guide <- function(horizontal, order = 1) {
  if (horizontal) {
    guide_legend(
      order = order,
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      label.position = if (.ggp_has_label_position) "bottom" else "right",
      override.aes = list(color="grey40", alpha=0.85)
    )
  } else {
    guide_legend(
      order = order,
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(color="grey40", alpha=0.85)
    )
  }
}

.gsea_make_colorbar_guide <- function(horizontal, reverse = FALSE, order = 2) {
  if (horizontal) {
    guide_colorbar(
      order = order,
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      label.position = if (.ggp_has_label_position) "bottom" else "right",
      barwidth = unit(6,"cm"),
      barheight = unit(0.5,"cm"),
      reverse = reverse
    )
  } else {
    guide_colorbar(
      order = order,
      direction = "vertical",
      reverse = reverse,
      label.hjust = 0.5
    )
  }
}

#' Apply legend theme - FINAL with improved horizontal spacing
.gsea_apply_legend_theme <- function(p, legend_position, sizes, rotate_when_horizontal = TRUE) {
  if (legend_position == "none") {
    return(p + theme(legend.position = "none"))
  }

  horizontal <- legend_position %in% c("top", "bottom")

  # Apply legend positioning
  p <- p + theme(
    legend.position = legend_position,
    legend.direction = if (horizontal) "horizontal" else "vertical",
    legend.box = if (horizontal) "horizontal" else "vertical",
    legend.box.just = if (horizontal) "center" else "left",        # LEFT for vertical box
    legend.justification = if (horizontal) "center" else c(0, 0.5), # LEFT for vertical
    legend.spacing.x = unit(8, "pt"),      # MORE horizontal spacing
    legend.spacing.y = unit(4, "pt"),      # Vertical spacing for multi-row
    legend.key.height = unit(16, "pt"),    # TALLER keys
    legend.key.width = unit(20, "pt"),     # WIDER keys for horizontal
    legend.text = element_text(
      size = sizes$legendText,
      angle = if (horizontal && rotate_when_horizontal) 45 else 0,
      hjust = if (horizontal && rotate_when_horizontal) 0.5 else if (!horizontal) 0 else 0.5,
      vjust = if (horizontal && rotate_when_horizontal) 0.5 else 0.5
    ),
    legend.title = element_text(
      size = sizes$legendTitle,
      hjust = if (!horizontal) 0 else 0.5  # LEFT align title for vertical
    )
  )

  return(p)
}

# ========================================
# Individual Plot Creation Functions
# ========================================

#' Create General Running Score Plot
create_general_running_score_plot <- function(gsea_results, pathway_idx, theme, sizes) {
  require(enrichplot)
  require(cowplot)

  p_list <- enrichplot::gseaplot(gsea_results$Results, by = "all", geneSetID = pathway_idx)

  # Apply theme and sizing
  p_list <- lapply(p_list, function(p) {
    p + theme +
      theme(
        axis.title = element_text(size = sizes$axisTitle),
        axis.text = element_text(size = sizes$tick),
        plot.margin = margin(1.5, 5.5, 1.5, 5.5, unit = "pt")
      )
  })

  rel_heights <- rep(1, length(p_list))
  panel_gap <- 1.5
  if (length(p_list) == 2L && requireNamespace("patchwork", quietly = TRUE)) {
    final_plot <- p_list[[1]] /
      patchwork::plot_spacer() /
      p_list[[2]] +
      patchwork::plot_layout(
        heights = grid::unit(c(1, panel_gap, 1), c("null", "pt", "null"))
      )
  } else {
    final_plot <- cowplot::plot_grid(
      plotlist    = p_list,
      ncol        = 1,
      align       = "v",
      rel_heights = rel_heights,
      greedy      = TRUE
    )
  }
  attr(final_plot, "gsea_running_score_plotlist") <- p_list
  attr(final_plot, "gsea_running_score_rel_heights") <- rel_heights
  attr(final_plot, "gsea_running_score_panel_gap") <- panel_gap
  class(final_plot) <- unique(c("gsea_running_score_alignable", class(final_plot)))

  pathway_name <- as.data.frame(gsea_results$Results)$Description[pathway_idx]
  message <- paste("General Running Score Plot for", pathway_name)

  return(list(plot = final_plot, message = message))
}

#' Create Running Score Plot - Mit diskontinuierlicher UI-Farbskala
create_running_score_plot <- function(gsea_results, pathway_ids, theme, sizes, legend_position="right", colors) {
  require(cowplot); require(enrichplot); require(ggplot2)

  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "GSEA RUNNING", message)
    } else {
      effective_level <- get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE)
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ GSEA RUNNING ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  # LIMIT to maximum 8 pathways
  if (length(pathway_ids) > 8) {
    debug_log(paste("Limiting pathways from", length(pathway_ids), "to 8 for better display"), 1)
    pathway_ids <- pathway_ids[1:8]
  }

  debug_log(paste("Creating running score plot with UI colors:", paste(colors, collapse = ", ")), 1)
  debug_log(paste("Legend position:", legend_position), 1)

  df <- as.data.frame(gsea_results$Results)

  pathway_names <- df$Description[pathway_ids]
  pathway_nes <- df$NES[pathway_ids]

  sorted_indices <- order(pathway_nes)
  pathway_ids_sorted <- pathway_ids[sorted_indices]
  pathway_names_sorted <- pathway_names[sorted_indices]
  pathway_nes_sorted <- pathway_nes[sorted_indices]

  debug_log(paste("Original pathway order:", paste(pathway_names, collapse = ", ")), 2)
  debug_log(paste("Original NES values:", paste(round(pathway_nes, 3), collapse = ", ")), 2)
  debug_log(paste("Sorted pathway order (by NES):", paste(pathway_names_sorted, collapse = ", ")), 2)
  debug_log(paste("Sorted NES values:", paste(round(pathway_nes_sorted, 3), collapse = ", ")), 2)

  pathway_colors <- create_gsea_pathway_color_gradient(colors, length(pathway_ids_sorted))
  names(pathway_colors) <- pathway_names_sorted

  debug_log(paste("Generated UI-based pathway colors:", paste(pathway_colors, collapse = ", ")), 2)
  debug_log("Color assignment: Lowest NES -> Color 1, Highest NES -> Color 3", 2)

  legend_order <- pathway_names_sorted
  legend_ids <- pathway_ids_sorted

  g3 <- enrichplot::gseaplot2(
    gsea_results$Results,
    geneSetID = legend_ids,
    pvalue_table = FALSE,
    ES_geom = "line"
  )

  if (length(pathway_ids) == 1) {
    debug_log("Single pathway mode detected - applying UI gradient to ranking metric", 1)

    for (i in seq_along(g3)) {
      # Plot-Level Aes
      aes_names <- tryCatch(paste(names(g3[[i]]$mapping), collapse = ","), error=function(e) "")
      scale_classes <- tryCatch(paste(sapply(g3[[i]]$scales$scales, function(s) class(s)[1]), collapse = ","), error=function(e) "")
      debug_log(paste("Single mode component", i, "plot aes:", ifelse(aes_names=="","<none>", aes_names)), 1)
      debug_log(paste("Single mode component", i, "plot scales:", ifelse(scale_classes=="","<none>", scale_classes)), 1)

      # Layer-Level Aes
      if (length(g3[[i]]$layers) > 0) {
        for (li in seq_along(g3[[i]]$layers)) {
          layer_aes_names <- tryCatch(paste(names(g3[[i]]$layers[[li]]$mapping), collapse=","), error=function(e) "")
          debug_log(paste("Component", i, "Layer", li, "aes:", ifelse(layer_aes_names=="","<none>", layer_aes_names)), 1)
        }
      } else {
        debug_log(paste("Component", i, "has no layers"), 1)
      }
    }

    grad_low  <- colors[1]
    grad_mid  <- colors[2]
    grad_high <- colors[3]

    for (i in seq_along(g3)) {
      # Layer-basierte Erkennung
      layer_has_colour <- any(vapply(g3[[i]]$layers, function(L) {
        any(c("colour","color") %in% names(L$mapping))
      }, logical(1)))
      layer_has_fill <- any(vapply(g3[[i]]$layers, function(L) {
        "fill" %in% names(L$mapping)
      }, logical(1)))

      # Falls trotzdem in plot$mapping definiert:
      plot_has_colour <- any(c("colour","color") %in% names(g3[[i]]$mapping))
      plot_has_fill   <- "fill" %in% names(g3[[i]]$mapping)

      apply_colour <- layer_has_colour || plot_has_colour
      apply_fill   <- layer_has_fill || plot_has_fill

      if (apply_colour) {
        g3[[i]] <- g3[[i]] + scale_color_gradient2(
          low = grad_low, mid = grad_mid, high = grad_high, midpoint = 0
        )
        debug_log(paste("Applied UI scale_color_gradient2 to component", i), 1)
      } else {
        debug_log(paste("No colour mapping detected for component", i, "- skipping color gradient"), 1)
      }

      if (apply_fill) {
        g3[[i]] <- g3[[i]] + scale_fill_gradient2(
          low = grad_low, mid = grad_mid, high = grad_high, midpoint = 0
        )
        debug_log(paste("Applied UI scale_fill_gradient2 to component", i), 1)
      } else {
        debug_log(paste("No fill mapping detected for component", i, "- skipping fill gradient"), 1)
      }
    }
  }

  style_sub <- function(p, type) {
    p + theme + theme(
      plot.margin = unit(c(5,5,5,5), "pt"),
      axis.title = element_text(size = sizes$axisTitle),
      axis.text = element_text(size = sizes$tick),
      legend.text = element_text(size = sizes$legendText),
      legend.title = element_text(size = sizes$legendTitle)
    )
  }

  p1 <- style_sub(g3[[1]], 1)
  p2 <- style_sub(g3[[2]], 2)
  p3 <- if (length(g3) >= 3) style_sub(g3[[3]], 3) else NULL

  p1 <- p1 + scale_color_manual(
    name = "Pathway",
    values = pathway_colors,
    labels = legend_order,
    breaks = legend_order,
    guide = guide_legend(reverse = TRUE) # (beibehalten, damit Legende zur p2-Reihenfolge passt)
  )

  p2 <- p2 + scale_color_manual(
    name = "Pathway",
    values = pathway_colors,
    labels = legend_order,
    breaks = legend_order
  ) + scale_y_discrete(limits = legend_order)

  legend_grob <- NULL
  if (legend_position != "none" && length(pathway_ids) > 1) {
    tryCatch({
      # Stabil aus "right", Inhalt linksbündig
      temp_plot <- p1 + theme(
        legend.position      = "right",
        legend.direction     = "vertical",
        legend.justification = c(0, 0.5),
        legend.text          = element_text(size = sizes$legendText,  hjust = 0),
        legend.title         = element_text(size = sizes$legendTitle, hjust = 0),
        legend.key.size      = unit(16, "pt"),
        legend.margin        = margin(5, 5, 5, 5),
        plot.margin          = margin(0, 0, 0, 0)
      )
      legend_grob <- cowplot::get_legend(temp_plot)
      debug_log("Successfully extracted legend with UI colors", 2)
    }, error = function(e) {
      debug_log(paste("Failed to extract legend:", e$message), 1)
    })
  }

  if (!is.null(legend_grob) && legend_position %in% c("top", "bottom") &&
      !identical(attr(legend_grob, "wrapped_top_bottom"), TRUE)) {
    if (legend_position == "top") {
      legend_grob <- cowplot::ggdraw() +
        cowplot::draw_grob(legend_grob,
                           x = 0.47, y = 0.35,   # leicht nach links
                           width = 1, height = 1,
                           halign = 0, valign = 0.5) +
        theme(plot.margin = margin(80, 0, 0, 0))
    } else { # bottom
      legend_grob <- cowplot::ggdraw() +
        cowplot::draw_grob(legend_grob,
                           x = 0.47, y = 0.78,   # leicht nach links
                           width = 1, height = 1,
                           halign = 0, valign = 0.5) +
        theme(plot.margin = margin(80, 0, 0, 0))
    }
    attr(legend_grob, "wrapped_top_bottom") <- TRUE
  }

  p1 <- p1 + theme(legend.position = "none")
  p2 <- p2 + theme(legend.position = "none")
  if (!is.null(p3)) p3 <- p3 + theme(legend.position = "none")

  p1 <- p1 + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

  p2 <- p2 + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

  if (!is.null(p3)) {
    # p3 bleibt unverändert in Achsentexten
  }

  # Stack anpassen: Wenn nur ein Pathway, g3 kann andere Struktur haben.
  if (is.null(p3)) {
    stacked_plot <- cowplot::plot_grid(
      p1, p2,
      ncol = 1,
      align = "v",
      rel_heights = c(5, 2)
    )
  } else {
    stacked_plot <- cowplot::plot_grid(
      p1, p2, p3,
      ncol = 1,
      align = "v",
      rel_heights = c(5, 1, 2)
    )
  }

  if (!is.null(legend_grob) && legend_position != "none") {
    legend_fraction <- switch(legend_position,
                              "top" = 0.30,
                              "bottom" = 0.35,
                              "left" = 0.35,
                              "right" = 0.35,
                              0.30
    )
    if (legend_position %in% c("top", "bottom")) {
      stacked_plot <- stacked_plot + theme(
        plot.margin = unit(c(15, 15, 15, 15), "pt")
      )
    }
    final_plot <- .gsea_place_legend_external(stacked_plot, legend_grob, legend_position, legend_fraction)
    debug_log(paste("Successfully added external legend with generous", legend_fraction, "fraction"), 1)
  } else {
    final_plot <- stacked_plot
  }

  display_names <- paste(pathway_names, collapse = ", ")
  debug_log(paste("Running score plot created with UI colors for", length(pathway_ids), "pathways (max 8)"), 1)
  return(list(plot = final_plot, message = paste("Running score for:", display_names)))
}

#' Create Enhanced Enrichment Dotplot - CORRECTED legend order: Gene Set Size first, then Adjusted p-Value
create_enrichment_dotplot <- function(gsea_results, selected_pathways, colors, theme, sizes, legend_position = "right", swap_panels = FALSE, y_ticks_right = FALSE) {
  require(ggplot2); require(dplyr)

  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "GSEA DOTPLOT", message)
    } else {
      effective_level <- get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE)
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ GSEA DOTPLOT ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  if (!length(selected_pathways)) {
    return(list(plot=NULL, message="Select at least one pathway for dotplot"))
  }

  debug_log(paste("Creating enrichment dotplot with legend position:", legend_position), 1)

  df <- as.data.frame(gsea_results$Results)
  df <- df[df$Description %in% selected_pathways, ]

  if (!all(c("Description","NES","p.adjust","setSize") %in% names(df))) {
    return(list(plot=NULL, message="Required columns missing"))
  }

  # Apply smart text wrapping
  df$wrapped <- gsea_smart_wrap_pathways(gsub("_"," ", df$Description), 30, 3)
  format_params <- gsea_determine_smart_pvalue_format(df$p.adjust)

  # Keep clusterProfiler-like facet semantics
  category_levels <- if (isTRUE(swap_panels)) {
    c("Suppressed", "Activated")
  } else {
    c("Activated", "Suppressed")
  }

  df <- df %>%
    mutate(
      Category = ifelse(NES > 0, "Activated", "Suppressed"),
      Category = factor(Category, levels = category_levels),
      wrapped = factor(wrapped, levels = rev(unique(wrapped)))
    )

  horizontal <- legend_position %in% c("top", "bottom")
  y_axis_position <- if (isTRUE(y_ticks_right)) "right" else "left"

  p <- ggplot(df, aes(NES, wrapped)) +
    geom_point(aes(size = setSize, color = p.adjust), alpha = 0.85) +
    scale_size_continuous(
      name = "Gene Set Size",
      range = c(3, 8),
      guide = if (horizontal) {
        guide_legend(
          order = 1,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom"
        )
      } else {
        guide_legend(order = 1, direction = "vertical")
      }
    ) +
    scale_color_gradientn(
      colors = colors,
      name = "Adjusted p-Value",
      labels = function(b) gsea_format_pvalues_smart(b, format_params),
      guide = if (horizontal) {
        guide_colorbar(
          order = 2,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          reverse = TRUE,
          barwidth = unit(6, "cm"),
          barheight = unit(0.5, "cm")
        )
      } else {
        guide_colorbar(order = 2, direction = "vertical", reverse = TRUE)
      }
    ) +
    scale_y_discrete(position = y_axis_position) +
    facet_wrap(~Category, scales = "free_x", nrow = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
    labs(x = "Normalized Enrichment Score (NES)", y = NULL) +
    theme +
    theme(
      axis.title = element_text(size = sizes$axisTitle),
      axis.text = element_text(size = sizes$tick),
      axis.title.y = element_blank(),
      strip.text = element_text(size = sizes$label, face = "bold"),
      panel.spacing.x = unit(1.5, "pt"),
      legend.position = legend_position,
      legend.text = element_text(
        size = sizes$legendText,
        angle = if (horizontal) 45 else 0,
        hjust = if (horizontal) 0.5 else if (legend_position %in% c("left", "right")) 0 else 0.5,
        vjust = 0.5
      ),
      legend.title = element_text(
        size = sizes$legendTitle,
        hjust = if (legend_position %in% c("left", "right")) 0 else 0.5
      )
    )

  # Managed class for Grid alignment policy (Concept C grouping)
  class(p) <- unique(c("gsea_enrichment_dotplot_facet_managed", class(p)))

  debug_log("Enrichment dotplot created as managed facet plot", 1)
  return(list(plot = p, message = "Dotplot created"))
}

# ========================================
# Cnet/Enrichment Map Specific Legend Helpers - Multi-row for horizontal
# ========================================

#' Apply Cnet/Enrichment Map specific legend theme
.gsea_apply_cnet_legend_theme <- function(p, legend_position, sizes, rotate_when_horizontal = TRUE) {
  if (legend_position == "none") {
    return(p + theme(legend.position = "none"))
  }

  horizontal <- legend_position %in% c("top", "bottom")

  # Apply legend positioning with enhanced spacing for Cnet plots
  p <- p + theme(
    legend.position = legend_position,
    legend.direction = if (horizontal) "horizontal" else "vertical",
    legend.box = if (horizontal) "horizontal" else "vertical",
    legend.box.just = if (horizontal) "center" else "left",        # LEFT for vertical box
    legend.justification = if (horizontal) "center" else c(0, 0.5), # LEFT for vertical
    legend.spacing.x = unit(10, "pt"),     # MORE horizontal spacing for Cnet
    legend.spacing.y = unit(6, "pt"),      # More vertical spacing for multi-row
    legend.key.height = unit(18, "pt"),    # TALLER keys for Cnet
    legend.key.width = unit(24, "pt"),     # WIDER keys for Cnet horizontal
    legend.text = element_text(
      size = sizes$legendText,
      angle = if (horizontal && rotate_when_horizontal) 45 else 0,
      hjust = if (horizontal && rotate_when_horizontal) 0.5 else if (!horizontal) 0 else 0.5,
      vjust = if (horizontal && rotate_when_horizontal) 0.5 else 0.5
    ),
    legend.title = element_text(
      size = sizes$legendTitle,
      hjust = if (!horizontal) 0 else 0.5  # LEFT align title for vertical
    )
  )

  return(p)
}

#' Create Cnet Plot with log2FC - Legend configuration as FINAL step
create_cnet_plot_fc <- function(gsea_results, selected_pathways, colors, theme, sizes, legend_position="right") {
  require(enrichplot)

  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "GSEA CNET FC", message)
    } else {
      effective_level <- get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE)
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ GSEA CNET FC ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  if (!length(selected_pathways)) return(list(plot=NULL, message="Select pathways"))

  fc <- gsea_results$GeneList_FC
  if (is.null(fc)) fc <- gsea_results$GeneList

  debug_log(paste("Creating cnet plot (FC) with legend position:", legend_position), 1)

  horizontal <- legend_position %in% c("top", "bottom")

  # Create base plot
  p <- enrichplot::cnetplot(
    gsea_results$Results,
    foldChange = fc,
    showCategory = selected_pathways
  )

  # Apply text wrapping post-processing
  p <- apply_gsea_smart_wrap_post_processing(p, selected_pathways, "cnet_plot", "black", "#8F8F8F", function(...) {})

  # Apply UI colors and titles FIRST
  p <- p +
    scale_colour_gradientn(
      name = expression(log[2]*"(FC)"),
      colors = colors
    ) +
    scale_size_continuous(
      name = "Gene Set Size"
    ) +
    theme +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  # Apply legend configuration as FINAL step - NO further theme changes after this!
  if (horizontal) {
    p <- p + guides(
      size = guide_legend(
        order = 1,                     # FIRST: Gene Set Size
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        # Single row for Cnet as requested
        override.aes = list(color = "grey40", alpha = 0.85)
      ),
      colour = guide_colorbar(
        order = 2,                     # SECOND: Color scale
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        barwidth = unit(8, "cm"),
        barheight = unit(0.8, "cm"),
        reverse = FALSE
      )
    ) +
      theme(
        legend.position = legend_position,
        legend.text = element_text(
          size = sizes$legendText,
          angle = 45,
          hjust = 0.5,
          vjust = 0.5
        ),
        legend.title = element_text(
          size = sizes$legendTitle,
          hjust = 0.5
        )
      )
  } else {
    p <- p + guides(
      size = guide_legend(
        order = 1,                     # FIRST: Gene Set Size
        direction = "vertical",
        title.position = "top",
        title.hjust = 0,
        override.aes = list(color = "grey40", alpha = 0.85)
      ),
      colour = guide_colorbar(
        order = 2,                     # SECOND: Color scale
        direction = "vertical",
        title.position = "top",
        title.hjust = 0,
        reverse = FALSE
      )
    ) +
      theme(
        legend.position = legend_position,
        legend.text = element_text(
          size = sizes$legendText,
          hjust = 0,
          vjust = 0.5
        ),
        legend.title = element_text(
          size = sizes$legendTitle,
          hjust = 0
        )
      )
  }

  debug_log("Cnet plot (FC) created with legend config as FINAL step", 1)
  return(list(plot = p, message = "Cnet plot (log2FC)"))
}


#' Create Enrichment Map - Legend configuration as FINAL step
create_enrichment_map <- function(gsea_results, selected_pathways, colors, theme, sizes, legend_position="right") {
  require(enrichplot)

  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "GSEA EMAP", message)
    } else {
      effective_level <- get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE)
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ GSEA EMAP ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  if (length(selected_pathways) < 3) {
    showNotification("Not enough pathways selected. Please select at least three pathways for the enrichment map.",
                     type = "error", duration = 5)

    return(list(plot = NULL, height = 600, width = 800, message = "Select at least 3 pathways for the enrichment map"))
    return(list(plot=NULL, message="Select at least 2 pathways"))
  }

  debug_log(paste("Creating enrichment map with legend position:", legend_position), 1)

  # Calculate pairwise similarity
  sim <- enrichplot::pairwise_termsim(gsea_results$Results)
  df <- as.data.frame(sim)
  pvals <- df$p.adjust[df$Description %in% selected_pathways]
  fmt <- gsea_determine_smart_pvalue_format(pvals)
  horizontal <- legend_position %in% c("top", "bottom")

  # Create base plot
  p <- enrichplot::emapplot(
    sim,
    showCategory = selected_pathways,
    color = "p.adjust",
    layout = "kk"
  )

  # Apply text wrapping post-processing
  p <- apply_gsea_smart_wrap_post_processing(p, selected_pathways, "enrichment_map", "black", debug_log = function(...) {})

  # Apply UI colors and titles FIRST
  p <- p +
    scale_color_gradientn(
      name = "Adjusted p-Value",
      colors = colors,
      labels = function(b) gsea_format_pvalues_smart(b, fmt)
    ) +
    scale_size_continuous(
      name = "Gene Set Size",
      range = c(3, 8)
    ) +
    theme +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  # Apply legend configuration as FINAL step - NO further theme changes after this!
  if (horizontal) {
    p <- p + guides(
      size = guide_legend(
        order = 1,                     # FIRST: Gene Set Size
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        nrow = 2,                      # Multi-row for Enrichment Map as requested
        byrow = TRUE,
        override.aes = list(color = "grey40", alpha = 0.85)
      ),
      color = guide_colorbar(
        order = 2,                     # SECOND: Adjusted p-Value
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        barwidth = unit(8, "cm"),
        barheight = unit(0.8, "cm"),
        reverse = TRUE
      )
    ) +
      theme(
        legend.position = legend_position,
        legend.text = element_text(
          size = sizes$legendText,
          angle = 45,
          hjust = 0.5,
          vjust = 0.5
        ),
        legend.title = element_text(
          size = sizes$legendTitle,
          hjust = 0.5
        )
      )
  } else {
    p <- p + guides(
      size = guide_legend(
        order = 1,                     # FIRST: Gene Set Size
        direction = "vertical",
        title.position = "top",
        title.hjust = 0,
        override.aes = list(color = "grey40", alpha = 0.85)
      ),
      color = guide_colorbar(
        order = 2,                     # SECOND: Adjusted p-Value
        direction = "vertical",
        title.position = "top",
        title.hjust = 0,
        reverse = TRUE
      )
    ) +
      theme(
        legend.position = legend_position,
        legend.text = element_text(
          size = sizes$legendText,
          hjust = 0,
          vjust = 0.5
        ),
        legend.title = element_text(
          size = sizes$legendTitle,
          hjust = 0
        )
      )
  }

  class(p) <- unique(c("gsea_enrichment_map_alignable", class(p)))

  debug_log("Enrichment map created with legend config as FINAL step", 1)
  return(list(plot = p, message = paste("Enrichment map with", length(selected_pathways), "pathways")))
}

#' External legend placement helper - IMPROVED with better spacing
.gsea_place_legend_external <- function(plot_grob, legend_grob, position, legend_fraction = 0.2) {
  require(cowplot)

  if (is.null(legend_grob) || position == "none") {
    return(plot_grob)
  }

  # IMPROVED: Better spacing calculations to prevent overlap and clipping
  switch(position,
         "top" = {
           # Add padding around legend for top position
           legend_with_padding <- cowplot::plot_grid(
             NULL, legend_grob, NULL,
             nrow = 1,
             rel_widths = c(0.1, 0.8, 0.1)  # Padding left and right
           )

           cowplot::plot_grid(
             legend_with_padding, plot_grob,
             ncol = 1,
             rel_heights = c(legend_fraction, 1 - legend_fraction),
             align = "v"
           )
         },
         "bottom" = {
           # Add padding around legend for bottom position
           legend_with_padding <- cowplot::plot_grid(
             NULL, legend_grob, NULL,
             nrow = 1,
             rel_widths = c(0.1, 0.8, 0.1)  # Padding left and right
           )

           cowplot::plot_grid(
             plot_grob, legend_with_padding,
             ncol = 1,
             rel_heights = c(1 - legend_fraction, legend_fraction),
             align = "v"
           )
         },
         "left" = {
           # Add padding around legend for left position
           legend_with_padding <- cowplot::plot_grid(
             NULL, legend_grob, NULL,
             ncol = 1,
             rel_heights = c(0.1, 0.8, 0.1)  # Padding top and bottom
           )

           cowplot::plot_grid(
             legend_with_padding, plot_grob,
             nrow = 1,
             rel_widths = c(legend_fraction, 1 - legend_fraction),
             align = "h"
           )
         },
         "right" = {
           # Add padding around legend for right position
           legend_with_padding <- cowplot::plot_grid(
             NULL, legend_grob, NULL,
             ncol = 1,
             rel_heights = c(0.1, 0.8, 0.1)  # Padding top and bottom
           )

           cowplot::plot_grid(
             plot_grob, legend_with_padding,
             nrow = 1,
             rel_widths = c(1 - legend_fraction, legend_fraction),
             align = "h"
           )
         },
         # Default case
         plot_grob
  )
}

#' Create PubMed Citation Plot - Enhanced with custom colors
create_pubmed_plot <- function(selected_pathways, colors, theme, sizes, legend_position="right") {
  require(enrichplot); require(patchwork); require(ggplot2)
  if (!length(selected_pathways)) return(list(plot=NULL,message="Select pathways for PubMed analysis"))
  last_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  yrs <- (last_year - 5):last_year
  pal <- colorRampPalette(colors)(length(selected_pathways))
  names(pal) <- selected_pathways

  base_theme <- theme +
    theme(axis.title=element_text(size=sizes$axisTitle),
          axis.text =element_text(size=sizes$tick),
          legend.title=element_text(size=sizes$legendTitle),
          legend.text =element_text(size=sizes$legendText))

  p1 <- enrichplot::pmcplot(selected_pathways, yrs) +
    scale_color_manual(values = pal, name = "Gene Set") +
    labs(x="Year", y="Proportion") +
    base_theme + theme(legend.position="none")
  p2 <- enrichplot::pmcplot(selected_pathways, yrs, proportion=FALSE) +
    scale_color_manual(values = pal, name = "Gene Set") +
    labs(x="Year", y="Count") +
    base_theme + theme(legend.position="none")

  layout <- (p1 / p2) + plot_layout(guides="collect", heights=c(1,1))
  final <- layout & theme(
    legend.position  = legend_position,
    legend.direction = if (legend_position %in% c("top","bottom")) "horizontal" else "vertical",
    legend.text      = element_text(size = sizes$legendText, angle = 0),
    legend.title     = element_text(size = sizes$legendTitle)
  )
  list(plot=final, message="PubMed citation analysis created")
}

# Source additional plot functions
if (file.exists("modules/GSEA/GSEA_plots_additional.R")) {
  source("modules/GSEA/GSEA_plots_additional.R", local = TRUE)
}

# ========================================
# Utility Functions
# ========================================

#' Get color limits for gradient scaling
get_color_limits <- function(values) {
  if (length(values) == 0) return(c(0, 0.5, 1))

  min_val <- min(values, na.rm = TRUE)
  max_val <- max(values, na.rm = TRUE)
  mid_val <- if (min_val == max_val) (min_val + max_val) / 2 else median(values, na.rm = TRUE)

  return(c(min_val, mid_val, max_val))
}

#' Get selected ggplot2 theme
get_selected_theme <- function(theme_name) {
  switch(theme_name,
         "Gray" = theme_gray(),
         "Black and White" = theme_bw(),
         "Linedraw" = theme_linedraw(),
         "Light" = theme_light(),
         "Dark" = theme_dark(),
         "Minimal" = theme_minimal(),
         "Classic" = theme_classic(),
         "Void" = theme_void(),
         theme_bw()  # Default
  )
}

# ========================================
# Additional Plot Creation Functions
# ========================================

#' Create Cnet Plot with Ranking Metrics - Legend configuration as FINAL step
create_cnet_plot_ranking <- function(gsea_results, selected_pathways, colors, theme, sizes, legend_position="right") {
  require(enrichplot)

  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "GSEA CNET RANK", message)
    } else {
      effective_level <- get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE)
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ GSEA CNET RANK ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  if (!length(selected_pathways)) return(list(plot=NULL, message="Select pathways"))

  rk <- gsea_results$GeneList
  if (is.null(rk)) return(list(plot=NULL, message="Ranking data not available"))

  debug_log(paste("Creating cnet plot (ranking) with legend position:", legend_position), 1)

  horizontal <- legend_position %in% c("top", "bottom")

  # Create base plot
  p <- enrichplot::cnetplot(
    gsea_results$Results,
    foldChange = rk,
    showCategory = selected_pathways
  )

  # Apply text wrapping post-processing
  p <- apply_gsea_smart_wrap_post_processing(p, selected_pathways, "cnet_plot", "black", "#8F8F8F", function(...) {})

  # Apply UI colors and titles FIRST
  p <- p +
    scale_colour_gradientn(
      name = "Ranking\nMetric",
      colors = colors
    ) +
    scale_size_continuous(
      name = "Gene Set Size"
    ) +
    theme +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  # Apply legend configuration as FINAL step - NO further theme changes after this!
  if (horizontal) {
    p <- p + guides(
      size = guide_legend(
        order = 1,                     # FIRST: Gene Set Size
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        # Single row for Cnet as requested
        override.aes = list(color = "grey40", alpha = 0.85)
      ),
      colour = guide_colorbar(
        order = 2,                     # SECOND: Color scale
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        barwidth = unit(8, "cm"),
        barheight = unit(0.8, "cm"),
        reverse = FALSE
      )
    ) +
      theme(
        legend.position = legend_position,
        legend.text = element_text(
          size = sizes$legendText,
          angle = 45,
          hjust = 0.5,
          vjust = 0.5
        ),
        legend.title = element_text(
          size = sizes$legendTitle,
          hjust = 0.5
        )
      )
  } else {
    p <- p + guides(
      size = guide_legend(
        order = 1,                     # FIRST: Gene Set Size
        direction = "vertical",
        title.position = "top",
        title.hjust = 0,
        override.aes = list(color = "grey40", alpha = 0.85)
      ),
      colour = guide_colorbar(
        order = 2,                     # SECOND: Color scale
        direction = "vertical",
        title.position = "top",
        title.hjust = 0,
        reverse = FALSE
      )
    ) +
      theme(
        legend.position = legend_position,
        legend.text = element_text(
          size = sizes$legendText,
          hjust = 0,
          vjust = 0.5
        ),
        legend.title = element_text(
          size = sizes$legendTitle,
          hjust = 0
        )
      )
  }

  debug_log("Cnet plot (ranking) created with legend config as FINAL step", 1)
  return(list(plot = p, message = "Cnet plot (Ranking Metrics)"))
}

#' Create Heatmap with log2FC - UPDATED with rotated labels
create_heatmap_fc <- function(gsea_results, selected_pathways, colors, theme, sizes, legend_position="right") {
  require(enrichplot)
  if (!length(selected_pathways)) return(list(plot=NULL,message="Select pathways"))
  fc <- gsea_results$GeneList_FC; if (is.null(fc)) fc <- gsea_results$GeneList
  out <- tryCatch({
    p <- enrichplot::heatplot(gsea_results$Results, foldChange = fc, showCategory = selected_pathways) +
      scale_fill_gradientn(name = expression(log[2]*"(FC)"), colors = colors) +
      theme +
      theme(axis.title=element_text(size=sizes$axisTitle),
            axis.text=element_text(size=sizes$tick),
            axis.text.x=element_text(size=sizes$tick, angle=45, hjust=1),
            legend.title=element_text(size=sizes$legendTitle),
            legend.text=element_text(size=sizes$legendText))
    if (!is.null(legend_position) && legend_position!="right") {
      if (legend_position=="none") {
        p <- p + theme(legend.position="none")
      } else if (legend_position %in% c("top","bottom")) {
        p <- p + theme(legend.position=legend_position,
                       legend.direction="horizontal") +
          guides(fill = build_horizontal_colorbar(expression(log[2]*"(FC)"), TRUE, reverse = FALSE))
      } else {
        p <- p + theme(legend.position=legend_position)
      }
    }
    list(plot=p, message="Heatmap (log2FC)")
  }, error=function(e) list(plot=NULL, message=paste("Error:",e$message)))
  out
}

#' Create Heatmap with Ranking Metrics - UPDATED with rotated labels
create_heatmap_ranking <- function(gsea_results, selected_pathways, colors, theme, sizes, legend_position="right") {
  require(enrichplot)
  if (!length(selected_pathways)) return(list(plot=NULL,message="Select pathways"))
  rk <- gsea_results$GeneList; if (is.null(rk)) return(list(plot=NULL,message="Ranking data not available"))
  out <- tryCatch({
    p <- enrichplot::heatplot(gsea_results$Results, foldChange = rk, showCategory = selected_pathways) +
      scale_fill_gradientn(name="Ranking\nMetric", colors = colors) +
      theme +
      theme(axis.title=element_text(size=sizes$axisTitle),
            axis.text=element_text(size=sizes$tick),
            axis.text.x=element_text(size=sizes$tick, angle=45, hjust=1),
            legend.title=element_text(size=sizes$legendTitle),
            legend.text=element_text(size=sizes$legendText))
    if (!is.null(legend_position) && legend_position!="right") {
      if (legend_position=="none") {
        p <- p + theme(legend.position="none")
      } else if (legend_position %in% c("top","bottom")) {
        p <- p + theme(legend.position=legend_position,
                       legend.direction="horizontal") +
          guides(fill = build_horizontal_colorbar("Ranking\nMetric", TRUE, reverse = FALSE))
      } else {
        p <- p + theme(legend.position=legend_position)
      }
    }
    list(plot=p, message="Heatmap (Ranking Metrics)")
  }, error=function(e) list(plot=NULL, message=paste("Error:",e$message)))
  out
}


#' #' Create Upset Plot for pathway overlaps
#' create_upset_plot <- function(gsea_results, selected_pathways, theme, sizes) {
#'   require(enrichplot)
#'
#'   if (length(selected_pathways) < 2) {
#'     return(list(plot = NULL, message = "Select at least 2 pathways for upset plot"))
#'   }
#'
#'   tryCatch({
#'     plot_obj <- enrichplot::upsetplot(
#'       gsea_results$Results,
#'       n = length(selected_pathways)
#'     ) +
#'       theme +
#'       theme(
#'         axis.title = element_text(size = sizes$axisTitle),
#'         axis.text = element_text(size = sizes$tick),
#'         plot.title = element_text(size = sizes$legendTitle)
#'       )
#'
#'     return(list(plot = plot_obj, message = "Upset plot created"))
#'
#'   }, error = function(e) {
#'     return(list(plot = NULL, message = paste("Error creating upset plot:", e$message)))
#'   })
#' }

#' Create Ridgeline Plot (Updated - Proper X-axis)
create_ridgeline_plot <- function(gsea_results, selected_pathways, colors, theme, sizes, legend_position="right") {
  require(enrichplot); require(ggridges)
  if (!length(selected_pathways)) return(list(plot=NULL,message="Select pathways"))
  out <- tryCatch({
    if (exists("ridgeplot", where=asNamespace("enrichplot"), inherits=FALSE)) {
      p <- enrichplot::ridgeplot(gsea_results$Results, showCategory = selected_pathways)
      p <- apply_gsea_smart_wrap_post_processing(p, selected_pathways, "ridgeline", "black", debug_log=function(...) {})
      p <- p +
        scale_fill_gradientn(colors = colors, name = "Adjusted p-Value") +
        labs(x="Enrichment Score") +
        theme +
        theme(axis.title=element_text(size=sizes$axisTitle),
              axis.text=element_text(size=sizes$tick),
              legend.title=element_text(size=sizes$legendTitle),
              legend.text=element_text(size=sizes$legendText))
    } else {
      df <- as.data.frame(gsea_results$Results)
      df <- df[df$Description %in% selected_pathways, ]
      df$wrapped <- gsea_smart_wrap_pathways(gsub("_"," ", df$Description), 40, 3)
      p <- ggplot(df, aes(x=enrichmentScore, y=wrapped, fill=NES)) +
        geom_density_ridges(alpha=0.7) +
        scale_fill_gradientn(name="NES", colors=colors) +
        labs(x="Enrichment Score", y="Pathway") +
        theme +
        theme(axis.title=element_text(size=sizes$axisTitle),
              axis.text=element_text(size=sizes$tick),
              legend.title=element_text(size=sizes$legendTitle),
              legend.text=element_text(size=sizes$legendText))
    }
    if (!is.null(legend_position) && legend_position!="right") {
      if (legend_position=="none") {
        p <- p + theme(legend.position="none")
      } else if (legend_position %in% c("top","bottom")) {
        p <- p + theme(legend.position=legend_position,
                       legend.direction="horizontal",
                       legend.box.margin=margin(t=4,b=4),
                       legend.text=element_text(angle=45, hjust=0.5, vjust=0.5)) +
          guides(fill = build_horizontal_colorbar(p$labels$fill %||% "Value", TRUE, reverse = FALSE))
      } else {
        p <- p + theme(legend.position=legend_position)
      }
    }
    list(plot=p, message="Ridgeline plot created")
  }, error=function(e) list(plot=NULL, message=paste("Error:",e$message)))
  out
}

# ========================================
# Helper Functions for Enhanced Plotting
# ========================================

#' Validate pathway selection for specific plot types
validate_pathway_selection <- function(plot_type, selected_pathways) {
  min_pathways <- switch(plot_type,
                         "Enrichment map" = 2,
                         "Upset plot" = 2,
                         1  # Default minimum
  )

  if (length(selected_pathways) < min_pathways) {
    return(list(
      valid = FALSE,
      message = paste("This plot type requires at least", min_pathways, "pathways")
    ))
  }

  return(list(valid = TRUE, message = ""))
}

#' Extract gene set overlap information
get_pathway_overlap_data <- function(gsea_results, selected_pathways) {
  results_df <- as.data.frame(gsea_results$Results)
  selected_data <- results_df[results_df$Description %in% selected_pathways, ]

  # Extract gene lists for each pathway
  gene_lists <- list()
  for (i in seq_len(nrow(selected_data))) {
    pathway_name <- selected_data$Description[i]
    core_genes <- selected_data$core_enrichment[i]

    if (!is.na(core_genes) && core_genes != "") {
      gene_lists[[pathway_name]] <- unlist(strsplit(core_genes, "/"))
    }
  }

  return(gene_lists)
}

#' Create custom color palette based on data range
create_adaptive_color_palette <- function(data_values, base_colors) {
  if (length(data_values) == 0) return(base_colors)

  # Calculate data range
  data_range <- range(data_values, na.rm = TRUE)
  data_center <- median(data_values, na.rm = TRUE)

  # Adjust colors based on data characteristics
  if (all(data_values >= 0, na.rm = TRUE)) {
    # All positive values - use two-color gradient
    return(c(base_colors[2], base_colors[3]))
  } else if (all(data_values <= 0, na.rm = TRUE)) {
    # All negative values - use two-color gradient
    return(c(base_colors[1], base_colors[2]))
  } else {
    # Mixed values - use full three-color gradient
    return(base_colors)
  }
}

#' Format pathway names for display
format_pathway_names <- function(pathway_names, max_length = 50) {
  sapply(pathway_names, function(name) {
    if (nchar(name) > max_length) {
      paste0(substr(name, 1, max_length - 3), "...")
    } else {
      name
    }
  })
}

# ========================================
# Enhanced GSEA PubMed Plot with Custom Color Gradient
# ========================================

#' Create discontinuous color gradient for GSEA pathways
#'
#' Creates a color gradient between Low/Medium/High colors with as many steps as pathways
#' @param colors vector with Low, Medium, High colors from UI
#' @param n_pathways number of pathways for color steps
#' @return vector of colors
create_pathway_color_gradient <- function(colors, n_pathways) {
  debug_log(paste("Creating discontinuous color gradient for", n_pathways, "pathways"), 2)

  if (n_pathways <= 1) {
    return(colors[2])  # Use medium color for single pathway
  }

  if (n_pathways == 2) {
    return(c(colors[1], colors[3]))  # Use low and high
  }

  # Create discontinuous gradient: Low -> Medium -> High
  low_color <- colors[1]    # GSEAColorInput_down
  mid_color <- colors[2]    # GSEAColorInput_zero
  high_color <- colors[3]   # GSEAColorInput_up

  # Calculate how many colors we need for each segment
  if (n_pathways == 3) {
    return(c(low_color, mid_color, high_color))
  }

  # For more than 3 pathways, create segments
  half_pathways <- ceiling(n_pathways / 2)

  # Create gradient from low to medium
  low_to_mid <- colorRampPalette(c(low_color, mid_color))(half_pathways)

  # Create gradient from medium to high
  mid_to_high <- colorRampPalette(c(mid_color, high_color))(n_pathways - half_pathways + 1)

  # Combine, removing duplicate middle color
  full_gradient <- c(low_to_mid, mid_to_high[-1])

  debug_log(paste("Generated", length(full_gradient), "colors for pathway gradient"), 2)
  return(full_gradient[1:n_pathways])  # Ensure exact length
}

#' Create GSEA pathway color gradient - Diskontinuierlich wie PubMed Citations
#' @param colors vector mit Low, Medium, High colors vom UI
#' @param n_pathways Anzahl der GSEA pathways
#' @return vector of colors
create_gsea_pathway_color_gradient <- function(colors, n_pathways) {

  debug_log <- function(message, level = 1) {
    rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
    if (is.function(rec)) {
      rec(level, "GSEA COLOR GRADIENT", message)
    } else {
      effective_level <- get0("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE)
      if (is.numeric(effective_level) && effective_level >= level)
        cat(paste0("[ GSEA COLOR GRADIENT ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
    }
  }

  colors <- rev(colors)

  # NEU: Duplicate-Check
  if (length(unique(colors)) == 1) {
    debug_log("All provided UI colors identical - collapsing to single color vector", 2)
    if (n_pathways <= 1) {
      return(colors[1])
    } else {
      return(rep(colors[1], n_pathways))
    }
  }

  debug_log(paste("Creating diskontinuierliche color gradient for", n_pathways, "pathways"), 2)

  if (n_pathways <= 1) {
    return(colors[2])  # Use medium color for single pathway (beibehaltet)
  }

  if (n_pathways == 2) {
    return(c(colors[1], colors[3]))  # Use low and high
  }

  # Create diskontinuierliche gradient: Low -> Medium -> High
  low_color <- colors[1]    # GSEAColorInput_down (nach rev)
  mid_color <- colors[2]    # GSEAColorInput_zero
  high_color <- colors[3]   # GSEAColorInput_up

  if (n_pathways == 3) {
    return(c(low_color, mid_color, high_color))
  }

  half_pathways <- ceiling(n_pathways / 2)
  low_to_mid <- colorRampPalette(c(low_color, mid_color))(half_pathways)
  mid_to_high <- colorRampPalette(c(mid_color, high_color))(n_pathways - half_pathways + 1)
  full_gradient <- c(low_to_mid, mid_to_high[-1])

  debug_log(paste("Generated", length(full_gradient), "diskontinuierliche colors for gradient"), 2)
  return(full_gradient[1:n_pathways])
}
