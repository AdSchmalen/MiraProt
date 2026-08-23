# ==============================================================================
# 6. Plot Creation Functions
# ==============================================================================

#' Determine Smart Formatting for P-Values (Including Scientific Notation)
#'
#' Analyzes p-values and determines whether to use scientific notation
#' and how many significant digits to show
#' @param values numeric vector of p-values
#' @param debug_log logging function
#' @return list with formatting parameters
determine_smart_pvalue_format <- function(values, debug_log = function(message, level = 1) {}) {

  values <- values[!is.na(values) & is.finite(values) & values > 0]

  if (length(values) == 0) {
    debug_log("No valid values, using default format")
    return(list(use_scientific = FALSE, digits = 3, significant = 3))
  }

  min_val <- min(values)
  max_val <- max(values)
  log10_min <- log10(min_val)
  log10_max <- log10(max_val)

  debug_log(paste("P-value range:", format(min_val, scientific = TRUE), "to", format(max_val, scientific = TRUE)))
  debug_log(paste("Log10 range:", round(log10_min, 2), "to", round(log10_max, 2)))

  use_scientific <- FALSE
  digits <- 3
  significant <- 3

  if (min_val < 1e-10) {
    use_scientific <- TRUE
    significant <- 2
    debug_log("Using scientific notation: extremely small values detected")
  } else if (min_val < 1e-4) {
    use_scientific <- TRUE
    significant <- 3
    debug_log("Using scientific notation: small values detected")
  } else if ((log10_max - log10_min) > 3) {
    use_scientific <- TRUE
    significant <- 3
    debug_log("Using scientific notation: large range detected")
  } else {
    use_scientific <- FALSE

    if (max_val < 0.001) {
      digits <- 4
    } else if (max_val < 0.01) {
      digits <- 3
    } else if (max_val < 0.1) {
      digits <- 2
    } else {
      digits <- 2
    }
    debug_log(paste("Using decimal notation with", digits, "decimal places"))
  }

  return(list(
    use_scientific = use_scientific,
    digits = digits,
    significant = significant
  ))
}

#' Format P-Values with Smart Notation
#'
#' Formats p-values using the determined smart format
#' @param breaks numeric vector of p-values to format
#' @param format_params list from determine_smart_pvalue_format
#' @return character vector of formatted values
format_pvalues_smart <- function(breaks, format_params) {

  if (format_params$use_scientific) {
    formatted <- format(breaks,
                        scientific = TRUE,
                        digits = format_params$significant)

    formatted <- gsub("e\\+00$", "", formatted)
    formatted <- gsub("e-0", "e-", formatted)
    formatted <- gsub("e\\+", "e", formatted)

    return(formatted)
  } else {
    return(format(breaks,
                  nsmall = format_params$digits,
                  digits = format_params$digits,
                  scientific = FALSE))
  }
}

#' Smart Text Wrapping Function for GO Terms
#'
#' Breaks long GO term names at logical points (spaces) to improve readability
#' @param text_vector character vector of GO term names
#' @param max_chars maximum characters per line (default: 35)
#' @param max_lines maximum number of lines (default: 3)
#' @param debug_log logging function
#' @return character vector with line breaks inserted
smart_wrap_go_terms <- function(text_vector, max_chars = 35, max_lines = 3,
                                debug_log = function(message, level = 1) {}) {

  debug_log(paste("Processing", length(text_vector), "terms for smart wrapping"))

  sapply(text_vector, function(text) {
    if (nchar(text) <= max_chars) {
      return(text)
    }

    debug_log(paste("Wrapping long term:", substr(text, 1, 30), "..."))

    words <- strsplit(text, " ")[[1]]
    if (length(words) <= 1) {
      return(text)
    }

    lines <- character()
    current_line <- ""

    for (word in words) {
      test_line <- if (nchar(current_line) == 0) word else paste(current_line, word)

      if (nchar(test_line) <= max_chars) {
        current_line <- test_line
      } else {
        if (nchar(current_line) > 0) {
          lines <- c(lines, current_line)
        }
        current_line <- word

        if (length(lines) >= max_lines - 1) {
          break
        }
      }
    }

    if (nchar(current_line) > 0) {
      lines <- c(lines, current_line)
    }

    if (length(lines) >= max_lines && length(words) > length(unlist(strsplit(paste(lines, collapse = " "), " ")))) {
      lines[max_lines] <- paste(substr(lines[max_lines], 1, max_chars - 3), "...")
      lines <- lines[1:max_lines]
    }

    result <- paste(lines, collapse = "\n")
    debug_log(paste("Wrapped to", length(lines), "lines"))
    return(result)
  }, USE.NAMES = FALSE)
}

#' Create GO Dotplot - ORIGINAL CUSTOM IMPLEMENTATION + Legend Position
#' @param edo enrichGO result object
#' @param selected_terms vector of selected GO terms (default NULL)
#' @param colors color vector for gradient (default NULL)
#' @param sizes list of UI size parameters (default NULL)
#' @param theme ggplot2 theme object (default NULL)
#' @param legend_position legend position ("top", "bottom", "left", "right", "none")
#' @param debug_log logging function
create_go_dotplot <- function(edo, selected_terms, colors, sizes, theme, legend_position = "right",
                              debug_log = function(message, level = 1) {}) {
  require(ggplot2)
  require(dplyr)

  if (length(selected_terms) == 0) {
    return(list(plot = NULL, height = 600, width = 800, message = "Select GO terms for dotplot"))
  }

  debug_log(paste("Creating GO dotplot with legend position:", legend_position), 1)

  tryCatch({
    result_df <- as.data.frame(edo)

    result_df <- result_df[result_df$Description %in% selected_terms, ]

    if (nrow(result_df) == 0) {
      return(list(plot = NULL, height = 600, width = 800, message = "No matching GO terms found"))
    }

    wrapped_count <- 0
    result_df$wrapped <- sapply(result_df$Description, function(desc) {
      if (nchar(desc) > 50) {
        wrapped_count <<- wrapped_count + 1
        paste(strwrap(desc, width = 50), collapse = "\n")
      } else {
        desc
      }
    })

    result_df$wrapped <- factor(result_df$wrapped, levels = rev(unique(result_df$wrapped)))

    horizontal <- legend_position %in% c("top", "bottom")

    p <- ggplot(result_df, aes(x = GeneRatio, y = wrapped)) +
      geom_point(aes(size = Count, color = p.adjust), alpha = 0.85) +
      scale_size_continuous(
        name = "Gene Count",
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
          guide_legend(
            order = 1,
            direction = "vertical",
            title.position = "top",
            title.hjust = 0
          )
        }
      ) +
      scale_color_gradientn(
        colors = colors,
        name = "Adjusted p-Value",
        trans = "log10",
        labels = scales::trans_format("log10", scales::math_format(10^.x)),
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
          guide_colorbar(
            order = 2,
            direction = "vertical",
            title.position = "top",
            title.hjust = 0,
            reverse = TRUE
          )
        }
      ) +
      labs(
        x = "Gene Ratio",
        y = "GO Term"
      ) +
      theme +
      theme(
        axis.title = element_text(size = sizes$axisTitle),
        axis.text = element_text(size = sizes$tick),
        legend.text = element_text(size = sizes$legendText),
        legend.title = element_text(size = sizes$legendTitle)
      )

    if (horizontal) {
      p <- p + theme(
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
      p <- p + theme(
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

    if (legend_position %in% c("top", "bottom")) {
      p <- p + guides(
        size = guide_legend(
          order = 1,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          nrow = 1,
          byrow = TRUE,
          override.aes = list(alpha = 0.85)
        ),
        color = guide_colorbar(
          order = 2,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          barwidth = unit(4, "cm"),
          barheight = unit(0.6, "cm"),
          reverse = FALSE
        )
      ) +
        theme(
          legend.position = legend_position,
          legend.text  = element_text(size = sizes$legendText, angle = 0, hjust = 0.5, vjust = 0.5),
          legend.title = element_text(size = sizes$legendTitle, hjust = 0.5)
        )
    } else {
      p <- p + theme(legend.position = legend_position)
    }

    debug_log(paste("GO dotplot created with GSEA-style legend:", wrapped_count, "terms wrapped"), 1)

    return(list(plot = p, height = 600, width = 800,
                message = paste("GO dotplot created with", nrow(result_df), "terms (", wrapped_count, "wrapped)")))

  }, error = function(e) {
    debug_log(paste("GO dotplot creation error:", e$message), 1)
    return(list(plot = NULL, height = 600, width = 800, message = paste("Error:", e$message)))
  })
}

#' Create GO Cnet Plot - ORIGINAL VERSION + Legend Position Only
#' @param results_list GO results list
#' @param selected_terms vector of selected GO terms
#' @param colors color vector for gradient
#' @param sizes list of UI size parameters
#' @param theme ggplot2 theme object
#' @param legend_position legend position ("top", "bottom", "left", "right", "none")
#' @param debug_log logging function
create_go_cnet_plot_fc_fixed <- function(results_list, selected_terms, colors, sizes, theme,
                                         legend_position = "right",
                                         debug_log = function(message, level = 1) {}) {
  require(enrichplot)

  if (length(selected_terms) == 0) {
    return(list(plot = NULL, height = 600, width = 800, message = "Select GO terms for Cnet plot"))
  }

  debug_log(paste("Creating GO cnet plot with legend position:", legend_position), 1)

  tryCatch({
    edo <- results_list$Edo_GO
    if (is.null(edo)) {
      return(list(plot = NULL, height = 600, width = 800, message = "GO enrichment results not available"))
    }

    fc_data <- results_list$FC_data
    if (is.null(fc_data) || length(fc_data) == 0) {
      genes_in_selected <- unique(unlist(strsplit(edo@result$geneID[edo@result$Description %in% selected_terms], "/")))
      fc_data <- rnorm(length(genes_in_selected), mean = 0, sd = 1.5)
      names(fc_data) <- genes_in_selected
      debug_log("Using simulated fold changes for demonstration", 2)
    }

    horizontal <- legend_position %in% c("top", "bottom")

    p <- enrichplot::cnetplot(
      edo,
      foldChange = fc_data,
      showCategory = selected_terms
    )

    p <- p +
      scale_colour_gradientn(
        name = expression(log[2]*"(FC)"),
        colors = colors
      ) +
      scale_size_continuous(
        name = "Gene Count"
      ) +
      theme +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )

    if (horizontal) {
      p <- p + guides(
        size = guide_legend(
          order = 1,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          override.aes = list(color = "grey40", alpha = 0.85)
        ),
        colour = guide_colorbar(
          order = 2,
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
          order = 1,
          direction = "vertical",
          title.position = "top",
          title.hjust = 0,
          override.aes = list(color = "grey40", alpha = 0.85)
        ),
        colour = guide_colorbar(
          order = 2,
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

    if (legend_position %in% c("top", "bottom")) {
      p <- p + guides(
        size = guide_legend(
          order = 1,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          nrow = 1,
          byrow = TRUE,
          override.aes = list(alpha = 0.85)
        ),
        color = guide_colorbar(
          order = 2,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          barwidth = unit(4, "cm"),
          barheight = unit(0.6, "cm"),
          reverse = FALSE
        )
      ) +
        theme(
          legend.position = legend_position,
          legend.text  = element_text(size = sizes$legendText, angle = 0, hjust = 0.5, vjust = 0.5),
          legend.title = element_text(size = sizes$legendTitle, hjust = 0.5)
        )
    } else {
      p <- p + theme(legend.position = legend_position)
    }

    debug_log("GO Cnet plot created with GSEA-style single-row legend", 1)

    return(list(
      plot = p,
      height = 600,
      width = 800,
      message = paste("GO Cnet plot created with", length(selected_terms), "terms")
    ))

  }, error = function(e) {
    debug_log(paste("GO Cnet plot creation error:", e$message), 1)
    return(list(plot = NULL, height = 600, width = 800, message = paste("Error creating Cnet plot:", e$message)))
  })
}

#' Create GO Enrichment Map - Legend Position als LETZTER Schritt
#' @param results_list GO results list
#' @param selected_terms vector of selected GO terms
#' @param colors color vector for gradient
#' @param sizes list of UI size parameters
#' @param theme ggplot2 theme object
#' @param legend_position legend position ("top", "bottom", "left", "right", "none")
#' @param debug_log logging function
create_go_enrichment_map_fixed <- function(results_list, selected_terms, colors, sizes, theme,
                                           legend_position = "right",
                                           debug_log = function(message, level = 1) {}) {
  require(enrichplot)
  require(ggplot2)

  if (length(selected_terms) < 3) {

    showNotification("Not enough GO terms selected. Please select at least three GO terms for the enrichment map.",
                     type = "error", duration = 5)

    return(list(plot = NULL, height = 600, width = 800, message = "Select at least 3 GO terms for the enrichment map"))
  }

  debug_log(paste("Creating GO enrichment map with legend position:", legend_position), 1)

  tryCatch({
    edo_similarity <- results_list$Edop_GO
    if (is.null(edo_similarity)) {
      debug_log("Calculating pairwise term similarity", 2)
      edo_similarity <- enrichplot::pairwise_termsim(results_list$Edo_GO)
    }

    horizontal <- legend_position %in% c("top", "bottom")

    p <- enrichplot::emapplot(
      edo_similarity,
      showCategory = selected_terms,
      color = "p.adjust",
      layout = "kk"
    )

    p <- p +
      scale_color_gradientn(
        name = "Adjusted p-Value",
        colors = colors,
        trans = "log10",
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      scale_size_continuous(
        name = "Gene Count",
        range = c(3, 8)
      ) +
      theme +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )

    p <- apply_smart_wrap_layer_replacement(
      plot_obj    = p,
      original_terms = selected_terms,
      max_chars   = 30,
      max_lines   = 4,
      debug_log   = debug_log
    )

    if (horizontal) {
      p <- p + guides(
        size = guide_legend(
          order = 1,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          nrow = 1,
          byrow = TRUE,
          override.aes = list(color = "grey40", alpha = 0.85)
        ),
        color = guide_colorbar(
          order = 2,
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          barwidth = unit(4, "cm"),
          barheight = unit(0.6, "cm"),
          reverse = TRUE
        )
      ) +
        theme(
          legend.position = legend_position,
          legend.text = element_text(
            size = sizes$legendText,
            angle = 0,
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
          order = 1,
          direction = "vertical",
          title.position = "top",
          title.hjust = 0,
          override.aes = list(color = "grey40", alpha = 0.85)
        ),
        color = guide_colorbar(
          order = 2,
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

    bw_ref <- ggplot2::theme_bw()
    if (identical(theme$panel.grid.major, bw_ref$panel.grid.major) &&
        identical(theme$panel.grid.minor, bw_ref$panel.grid.minor)) {
      p <- p + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid      = element_blank()
      )
    }

    class(p) <- unique(c("go_enrichment_map_alignable", class(p)))

    debug_log("GO enrichment map created with GSEA-style multi-row legend", 1)

    return(list(
      plot = p,
      height = 600,
      width = 800,
      message = paste("GO enrichment map created with", length(selected_terms), "terms")
    ))

  }, error = function(e) {
    debug_log(paste("GO enrichment map creation error:", e$message), 1)
    return(list(plot = NULL, height = 600, width = 800, message = paste("Error creating enrichment map:", e$message)))
  })
}

#' Create GO PubMed Citation Plot - Legend Position als LETZTER Schritt
#' @param selected_terms vector of selected GO terms
#' @param colors color vector for gradient
#' @param sizes list of UI size parameters
#' @param theme ggplot2 theme object
#' @param legend_position legend position ("top", "bottom", "left", "right", "none")
#' @param debug_log logging function
#' @param progress_fn optional function accepting an absolute value and current-task detail
create_go_pubmed_plot <- function(selected_terms, colors, sizes, theme, legend_position = "right",
                                  debug_log = function(message, level = 1) {}, progress_fn = NULL) {
  require(enrichplot)
  require(ggplot2)

  debug_log("Creating GO PubMed citation plot with custom color gradient", 1)

  if (length(selected_terms) == 0) {
    return(list(plot = NULL, height = 600, width = 800, message = "Select GO terms for PubMed analysis"))
  }

  tryCatch({
    if (is.function(progress_fn)) progress_fn(0.05, "Preparing GO PubMed citation query")

    last_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
    year_range <- (last_year - 5):last_year

    debug_log(paste("Fetching PubMed data for years:", paste(year_range, collapse = ", ")), 2)
    debug_log(paste("Using custom colors for", length(selected_terms), "terms"), 1)

    term_colors <- create_go_term_color_gradient(colors, length(selected_terms))
    names(term_colors) <- selected_terms

    debug_log(paste("Generated color gradient:", paste(term_colors, collapse = ", ")), 2)

    if (is.function(progress_fn)) progress_fn(0.15, "Querying PubMed/PMC for citation proportions")
    plot1 <- enrichplot::pmcplot(selected_terms, year_range)

    if (is.function(progress_fn)) progress_fn(0.55, "Querying PubMed/PMC for citation counts")
    plot2 <- enrichplot::pmcplot(selected_terms, year_range, proportion = FALSE)

    if (is.function(progress_fn)) progress_fn(0.85, "Assembling GO PubMed citation plot")
    plot1 <- plot1 +
      scale_color_manual(values = term_colors) +
      labs(
        colour = "GO Term",
        x = "Year",
        y = "Proportion"
      ) +
      theme +
      theme(
        axis.title = element_text(size = sizes$axisTitle),
        axis.text = element_text(size = sizes$tick),
        legend.text = element_text(size = sizes$legendText),
        legend.title = element_text(size = sizes$legendTitle)
      )

    plot2 <- plot2 +
      scale_color_manual(values = term_colors) +
      labs(
        colour = "GO Term",
        x = "Year",
        y = "Count"
      ) +
      theme +
      theme(
        axis.title = element_text(size = sizes$axisTitle),
        axis.text = element_text(size = sizes$tick),
        legend.text = element_text(size = sizes$legendText),
        legend.title = element_text(size = sizes$legendTitle)
      )

    if (is.function(progress_fn)) progress_fn(0.97, "Applying GO PubMed plot styling")
    if (!is.null(legend_position) && legend_position != "right") {
      debug_log(paste("Applying legend position:", legend_position, "to BOTH plots AFTER all customizations"), 2)

      if (legend_position == "none") {
        plot1 <- plot1 + theme(legend.position = "none")
        plot2 <- plot2 + theme(legend.position = "none")
      } else {
        if (legend_position %in% c("top", "bottom")) {
          plot1 <- plot1 +
            theme(legend.position = legend_position) +
            guides(color = guide_legend(direction = "horizontal"))

          plot2 <- plot2 +
            theme(legend.position = legend_position) +
            guides(color = guide_legend(direction = "horizontal"))
        } else {
          plot1 <- plot1 +
            theme(legend.position = legend_position) +
            guides(color = guide_legend(direction = "vertical"))

          plot2 <- plot2 +
            theme(legend.position = legend_position) +
            guides(color = guide_legend(direction = "vertical"))
        }
      }
    }

    require(cowplot)
    combined_plot <- cowplot::plot_grid(plot1, plot2, ncol = 1, align = "hv")

    debug_log("GO PubMed citation plot with custom color gradient created successfully", 1)
    return(list(plot = combined_plot, height = 800, width = 800,
                message = "GO PubMed citation analysis with custom colors created"))

  }, error = function(e) {
    debug_log(paste("GO PubMed plot creation error:", e$message), 1)
    return(list(plot = NULL, height = 600, width = 800, message = paste("Error fetching PubMed data:", e$message)))
  })
}

# ==============================================================================
# 7. Theme and Legend Helpers
# ==============================================================================

#' Get selected ggplot2 theme - GO version
#' @param theme_name name of theme to use
#' @param debug_log logging function
get_selected_theme_go <- function(theme_name, debug_log = function(message, level = 1) {}) {
  tryCatch({
    switch(theme_name,
           "Gray" = theme_gray(),
           "Black and White" = theme_bw(),
           "Linedraw" = theme_linedraw(),
           "Light" = theme_light(),
           "Dark" = theme_dark(),
           "Minimal" = theme_minimal(),
           "Classic" = theme_classic(),
           "Void" = theme_void(),
           theme_bw()
    )
  }, error = function(e) {
    debug_log(paste("Error in theme selection:", e$message), 1)
    theme_bw()
  })
}

#' Apply Legend Position to Plot
#'
#' @param plot_obj ggplot object
#' @param legend_position position ("top", "bottom", "left", "right", "none")
#' @param debug_log logging function
#' @return updated ggplot object
apply_legend_position <- function(plot_obj, legend_position = "right",
                                  debug_log = function(message, level = 1) {}) {

  debug_log(paste("Applying legend position:", legend_position), 2)

  legend_direction <- if (legend_position %in% c("top", "bottom")) {
    "horizontal"
  } else if (legend_position %in% c("left", "right")) {
    "vertical"
  } else {
    "vertical"
  }

  debug_log(paste("Legend direction set to:", legend_direction), 2)

  if (legend_position == "none") {
    plot_obj <- plot_obj + theme(legend.position = "none")
  } else {
    plot_obj <- plot_obj +
      theme(legend.position = legend_position) +
      guides(
        color = guide_legend(direction = legend_direction),
        fill = guide_legend(direction = legend_direction),
        size = guide_legend(direction = legend_direction),
        alpha = guide_legend(direction = legend_direction),
        shape = guide_legend(direction = legend_direction)
      )
  }

  return(plot_obj)
}

#' Extract plot parameters from GO UI inputs - Updated with legend position
#'
#' @param input Shiny input object
#' @param debug_log logging function
#' @return list of plot parameters
extract_go_plot_parameters <- function(input, debug_log = function(message, level = 1) {}) {

  plot_type <- input$custom_EnrichPlot_select_GO %||% "Enrichment score dotplot"

  colors <- if (plot_type %in% c("Cnet plot (log2FC)", "Enrichment map", "Enrichment score dotplot")) {
    c(
      input$GOColorInput_down %||% "#440154FF",
      input$GOColorInput_zero %||% "#31688EFF",
      input$GOColorInput_up %||% "#EFC000FF"
    )
  } else {
    c("#440154FF", "#31688EFF", "#EFC000FF")
  }

  sizes <- list(
    axisTitle = as.numeric(input$AxisTitleSize_GO %||% 12),
    tick = as.numeric(input$tickSize_GO %||% 10),
    legendText = as.numeric(input$LegendTextSize_GO %||% 10),
    legendTitle = as.numeric(input$LegendTitleSize_GO %||% 12),
    label = as.numeric(input$LabelSize_GO %||% 12)
  )

  theme_name <- input$ThemeSelect_GO %||% "Black and White"
  theme <- get_selected_theme_go(theme_name)

  legend_position <- input$LegendPosition_GO %||% "right"

  plot_height <- input$plot_height_go %||% 600

  return(list(
    plot_type = plot_type,
    colors = colors,
    sizes = sizes,
    theme = theme,
    legend_position = legend_position,
    plot_height = plot_height
  ))
}

#' Calculate Current Plot Dimensions in Inches
#'
#' Calculate the current plot dimensions based on window size and plot container
#' @param plot_height_px current plot height in pixels
#' @param plot_width_px current plot width in pixels
#' @param window_dpi effective DPI (default 96 for web)
#' @return list with width and height in inches
calculate_current_plot_dimensions <- function(plot_height_px, plot_width_px, window_dpi = 96) {
  tryCatch({
    width_inches <- plot_width_px / window_dpi
    height_inches <- plot_height_px / window_dpi

    width_inches <- max(2, min(width_inches, 50))
    height_inches <- max(2, min(height_inches, 50))

    list(
      width = round(width_inches, 1),
      height = round(height_inches, 1)
    )
  }, error = function(e) {
    list(width = 10, height = 8)
  })
}

# ==============================================================================
# 10. Label Formatting and Layer Utilities
# ==============================================================================

#' Complete GeomTextRepel Layer Replacement for Smart Wrap
#'
#' @param plot_obj ggplot object
#' @param original_terms original term labels
#' @param max_chars maximum characters per line
#' @param max_lines maximum lines per label
#' @param debug_log logging function
#' @return updated ggplot object
apply_smart_wrap_layer_replacement <- function(plot_obj, original_terms, max_chars = 25,
                                               max_lines = 2,
                                               debug_log = function(message, level = 1) {}) {

  debug_log("Starting COMPLETE GeomTextRepel layer replacement", 1)

  tryCatch({
    wrapped_mapping <- setNames(
      smart_wrap_go_terms(original_terms, max_chars = max_chars, max_lines = max_lines),
      original_terms
    )

    wrapped_count <- sum(grepl("\n", wrapped_mapping))
    debug_log(paste("Created wrapped mapping:", wrapped_count, "terms wrapped"), 2)

    text_repel_layers <- c()
    for (i in seq_along(plot_obj$layers)) {
      layer_class <- class(plot_obj$layers[[i]]$geom)
      if ("GeomTextRepel" %in% layer_class) {
        text_repel_layers <- c(text_repel_layers, i)
      }
    }

    debug_log(paste("Found", length(text_repel_layers), "GeomTextRepel layers"), 1)

    if (length(text_repel_layers) == 0) {
      debug_log("No GeomTextRepel layers found - cannot apply smart wrap", 1)
      return(plot_obj)
    }

    original_layer <- plot_obj$layers[[text_repel_layers[1]]]

    plot_built <- ggplot_build(plot_obj)
    text_layer_data <- plot_built$data[[text_repel_layers[1]]]

    debug_log(paste("Extracted data from GeomTextRepel layer:", nrow(text_layer_data), "rows"), 2)
    debug_log(paste("Available columns:", paste(names(text_layer_data), collapse = ", ")), 2)

    if ("label" %in% names(text_layer_data)) {
      debug_log("Applying smart wrap to extracted label data", 1)

      text_layer_data$label <- sapply(text_layer_data$label, function(label) {
        if (label %in% names(wrapped_mapping)) {
          wrapped_mapping[[label]]
        } else {
          label
        }
      }, USE.NAMES = FALSE)

      updated_count <- sum(text_layer_data$label != sapply(original_terms, function(x) x, USE.NAMES = FALSE))
      debug_log(paste("Updated", updated_count, "labels with smart wrap"), 2)

      debug_log("Removing original GeomTextRepel layer", 2)
      plot_obj$layers[[text_repel_layers[1]]] <- NULL

      debug_log("Adding new GeomTextRepel layer with wrapped labels", 1)

      require(ggrepel)

      new_layer <- ggrepel::geom_text_repel(
        data = text_layer_data,
        mapping = aes(x = x, y = y, label = label),
        size = 4.2,
        color = text_layer_data$colour[1] %||% "black",
        hjust = 0.5,
        vjust = 0.5,
        max.overlaps = Inf,
        show.legend = FALSE
      )

      plot_obj <- plot_obj + new_layer

      debug_log("Layer replacement completed successfully", 1)

    } else {
      debug_log("No 'label' column found in GeomTextRepel data", 1)
    }

    return(plot_obj)

  }, error = function(e) {
    debug_log(paste("Layer replacement failed:", e$message), 1)
    debug_log("Returning original plot", 2)
    return(plot_obj)
  })
}

#' Create discontinuous color gradient for GO terms
#'
#' Creates a color gradient between Low/Medium/High colors with as many steps as GO terms
#' @param colors vector with Low, Medium, High colors from UI
#' @param n_terms number of GO terms for color steps
#' @param debug_log logging function
#' @return vector of colors
create_go_term_color_gradient <- function(colors, n_terms,
                                          debug_log = function(message, level = 1) {}) {
  debug_log(paste("Creating discontinuous color gradient for", n_terms, "terms"), 2)

  if (n_terms <= 1) {
    return(colors[2])
  }

  if (n_terms == 2) {
    return(c(colors[1], colors[3]))
  }

  low_color <- colors[1]
  mid_color <- colors[2]
  high_color <- colors[3]

  if (n_terms == 3) {
    return(c(low_color, mid_color, high_color))
  }

  half_terms <- ceiling(n_terms / 2)

  low_to_mid <- colorRampPalette(c(low_color, mid_color))(half_terms)
  mid_to_high <- colorRampPalette(c(mid_color, high_color))(n_terms - half_terms + 1)

  full_gradient <- c(low_to_mid, mid_to_high[-1])

  debug_log(paste("Generated", length(full_gradient), "colors for gradient"), 2)
  return(full_gradient[1:n_terms])
}
