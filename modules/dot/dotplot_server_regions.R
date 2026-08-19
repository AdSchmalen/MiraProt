# ==============================================================================
# dotplot_server_regions.R - Dotplot region observers and helpers
#
# Purpose: Hosts region matrix rendering, region styling observers, and helper
# utilities used by the Dotplot module.
#
# Structure:
#   - Region matrix output and dynamic region button observers
#   - Region style application and live refresh observers
#   - Helper functions for region matrix labels and point styling
#
# Dependencies: shiny, shinyjs, colourpicker
# Called by: modDotPlotServer(), dotplot_utils.R
# ==============================================================================

# ------------------------------------------------------------------------------
# dotplot_init_region_observers
# Purpose: Initializes region observers and outputs for Dotplot.
# Structure:
#   - Section 1: Render region matrix and setup dynamic region selection.
#   - Section 2: Show selected region details and apply style settings.
#   - Section 3: Trigger live plot updates for region styling controls.
# Parameters:
#   - input: [reactivevalues] - Module input values.
#   - output: [shinyoutput] - Module output bindings.
#   - session: [shinysession] - Active module session.
#   - ns: [function] - Namespace helper.
#   - dotplot_debug_log: [function] - Dotplot debug logger from parent module.
#   - dotplot_state: [reactivevalues] - Dotplot state container.
#   - data_in: [reactive] - Input data reactive.
#   - data_def_in: [reactive] - Metadata reactive.
#   - region_configs: [reactiveVal] - Region style configuration store.
#   - selected_region: [reactiveVal] - Currently selected region key.
#   - region_structure: [reactive] - Region matrix metadata.
# Returns: Invisible NULL.
# ------------------------------------------------------------------------------
dotplot_init_region_observers <- function(input, output, session, ns, dotplot_debug_log, dotplot_state, data_in, data_def_in,
                                          region_configs, selected_region, region_structure) {
previous_region_structure <- reactiveVal(NULL)

restored_cache_authoritative <- function() {
  isTRUE(isolate(dotplot_state$plot_from_restore_cache)) &&
    is.list(isolate(dotplot_state$restore_plot_data_cache))
}


# Apply deferred MA region styles only after threshold-based region matrix is available
observeEvent(region_structure(), {
  structure <- region_structure()

  # During session restore the restored region_configs are already keyed for
  # the restored thresholds/structure and are the authoritative source of
  # truth.  Leaving this observer to run would wipe them (it unconditionally
  # clears styles for valid tile keys before re-applying the pending MA
  # preset).  Skip while the restore guard is raised and advance
  # previous_region_structure so the downstream remap observer also sees no
  # structural change.
  if (isTRUE(dotplot_state$restore_in_progress) || restored_cache_authoritative()) {
    previous_region_structure(structure)
    return()
  }

  pending_styles <- dotplot_state$pending_ma_region_styles

  if (is.null(pending_styles) || !is.list(pending_styles) || length(pending_styles) == 0) {
    return()
  }

  # Require at least 2 rows (intensity split) and 3 columns (abundance ratio split)
  if (is.null(structure$n_h_regions) || is.null(structure$n_v_regions) ||
      structure$n_h_regions < 2 || structure$n_v_regions < 3) {
    return()
  }

  valid_region_keys <- as.vector(outer(
    seq_len(structure$n_h_regions),
    seq_len(structure$n_v_regions),
    FUN = function(h, v) paste0(h, "_", v)
  ))

  current_configs <- isolate(region_configs())
  if (is.null(current_configs) || !is.list(current_configs)) {
    current_configs <- list()
  }

  # For MA preset defaults: clear existing styles for currently valid tiles first,
  # then apply only the requested MA tile colors.
  current_configs <- current_configs[setdiff(names(current_configs), valid_region_keys)]

  for (region_key in names(pending_styles)) {
    if (region_key %in% valid_region_keys) {
      current_configs[[region_key]] <- pending_styles[[region_key]]
    }
  }

  region_configs(current_configs)

  # Advance previous_region_structure to the current structure so the remap
  # observer (registered after this one) sees no structural change and skips
  # remap_region_configs_by_overlap.  Without this, the remap would use the
  # pre-MA old_structure to compute bounds for the freshly written MA region
  # keys, causing all tiles to inherit one region's color (e.g. all purple).
  previous_region_structure(structure)

  dotplot_state$pending_ma_region_styles <- NULL
  dotplot_debug_log("Applied deferred MA region styling after threshold tiles became available", 2)
}, ignoreInit = FALSE)

# Apply deferred Volcano region styles only after threshold-based region matrix is available
observeEvent(region_structure(), {
  structure <- region_structure()

  # Skip during session restore (see comment on the MA observer above) to
  # avoid wiping the restored region_configs via the pre-clear + re-apply
  # step below.
  if (isTRUE(dotplot_state$restore_in_progress) || restored_cache_authoritative()) {
    previous_region_structure(structure)
    return()
  }

  pending_styles <- dotplot_state$pending_volcano_region_styles

  if (is.null(pending_styles) || !is.list(pending_styles) || length(pending_styles) == 0) {
    return()
  }

  # Require at least 2 rows (p-value split) and 3 columns (fold-change split)
  if (is.null(structure$n_h_regions) || is.null(structure$n_v_regions) ||
      structure$n_h_regions < 2 || structure$n_v_regions < 3) {
    return()
  }

  valid_region_keys <- as.vector(outer(
    seq_len(structure$n_h_regions),
    seq_len(structure$n_v_regions),
    FUN = function(h, v) paste0(h, "_", v)
  ))

  current_configs <- isolate(region_configs())
  if (is.null(current_configs) || !is.list(current_configs)) {
    current_configs <- list()
  }

  # Clear existing styles for currently valid tiles, then apply only intended Volcano tile colors.
  current_configs <- current_configs[setdiff(names(current_configs), valid_region_keys)]

  for (region_key in names(pending_styles)) {
    if (region_key %in% valid_region_keys) {
      current_configs[[region_key]] <- pending_styles[[region_key]]
    }
  }

  region_configs(current_configs)

  # Advance previous_region_structure so the remap observer sees no structural change
  # and skips remap_region_configs_by_overlap, preventing full-plot recoloring.
  previous_region_structure(structure)

  dotplot_state$pending_volcano_region_styles <- NULL
  dotplot_debug_log("Applied deferred Volcano region styling after threshold tiles became available", 2)
}, ignoreInit = FALSE)

observeEvent(region_structure(), {
  current_structure <- region_structure()
  previous_structure <- previous_region_structure()

  # During session restore the region matrix "changes" from the module's
  # initial 1x1 default to whatever grid the restored thresholds imply.
  # The restored region_configs are already keyed for the restored grid
  # (authoritative source of truth), but the remap below would treat them
  # as if they belonged to `previous_structure` (= initial 1x1) and try to
  # migrate them onto `current_structure` (= restored grid).  Because the
  # intersection of the restored config keys with the 1x1 bounds is at
  # most {"1_1"}, the remap either collapses every tile to one colour or
  # returns an empty list -- wiping all per-region styling.  The restored
  # ggplot is still rendered (its colours are baked into scale_identity),
  # but any subsequent observer that re-reads region_configs (sync echo,
  # fallback plot regeneration, renderUI for the button matrix, later
  # user edits) sees the mangled configs and the custom region styling
  # disappears.
  #
  # Skip the remap while the restore guard is raised, but still advance
  # previous_region_structure / fix up selected_region so post-restore
  # threshold edits continue to remap correctly.
  if (isTRUE(dotplot_state$restore_in_progress) || restored_cache_authoritative()) {
    valid_region_keys <- as.vector(outer(
      seq_len(current_structure$n_h_regions),
      seq_len(current_structure$n_v_regions),
      FUN = function(h, v) paste0(h, "_", v)
    ))
    cur_sel <- selected_region()
    if (is.null(cur_sel) || !(cur_sel %in% valid_region_keys)) {
      if (length(valid_region_keys) > 0) {
        selected_region(valid_region_keys[[1]])
      }
    }
    previous_region_structure(current_structure)
    dotplot_debug_log(
      "Skipping region_configs remap during session restore; restored configs preserved", 1)
    return()
  }

  # Ensure selected region always points to a valid matrix cell.
  valid_region_keys <- as.vector(outer(seq_len(current_structure$n_h_regions), seq_len(current_structure$n_v_regions),
    FUN = function(h, v) paste0(h, "_", v)
  ))

  if (current_structure$total_regions == 1) {
    selected_region("1_1")
  } else if (is.null(selected_region()) || !(selected_region() %in% valid_region_keys)) {
    selected_region(valid_region_keys[[1]])
  }

  if (!is.null(previous_structure) && !identical(previous_structure, current_structure)) {
    migrated_configs <- remap_region_configs_by_overlap(
      old_configs = isolate(region_configs()),
      old_structure = previous_structure,
      new_structure = current_structure
    )
    region_configs(migrated_configs)
  }

  previous_region_structure(current_structure)
}, ignoreInit = FALSE)

output$region_button_matrix <- renderUI({
  structure <- region_structure()
  button_matrix <- create_region_button_matrix(structure, ns, region_configs())

  tagList(
    div(
      style = "margin-bottom: 10px; font-size: 12px; color: #666;",
      if (structure$total_regions == 1) {
        "Entire Plot Region"
      } else {
        paste0("Matrix: ", structure$n_h_regions, " × ", structure$n_v_regions,
               " (", structure$total_regions, " regions)")
      }
    ),
    button_matrix
  )
})

update_region_inputs <- function(region_key) {
  configs <- region_configs()

  if (!is.null(region_key) && region_key %in% names(configs)) {
    config <- configs[[region_key]]
    colourpicker::updateColourInput(session, "region_point_color", value = config$color)
    updateNumericInput(session, "region_point_size", value = config$size)
    updateNumericInput(session, "region_point_alpha", value = config$alpha)
    updateSelectInput(session, "region_point_shape", selected = config$shape)
  } else {
    colourpicker::updateColourInput(session, "region_point_color", value = "#E0E0E0")
    updateNumericInput(session, "region_point_size", value = 1)
    updateNumericInput(session, "region_point_alpha", value = 0.7)
    updateSelectInput(session, "region_point_shape", selected = 19)
  }
}

# Region button observers
observe({
  structure <- region_structure()

  for (h in 1:structure$n_h_regions) {
    for (v in 1:structure$n_v_regions) {
      local({
        h_local <- h
        v_local <- v
        button_id <- paste0("region_", h_local, "_", v_local)

        observeEvent(input[[button_id]], {
          selected_region(paste0(h_local, "_", v_local))
        })
      })
    }
  }
})

observeEvent(selected_region(), {
  update_region_inputs(selected_region())
}, ignoreInit = FALSE)

# Add hover effects when matrix is rendered
observe({
  structure <- region_structure()
  add_region_hover_effects(session, ns, structure)
})

# Selected region info
output$selected_region_info <- renderText({
  if (is.null(selected_region())) {
    return("")
  }

  structure <- region_structure()
  region_parts <- strsplit(selected_region(), "_")[[1]]
  h_pos <- as.numeric(region_parts[1])
  v_pos <- as.numeric(region_parts[2])

  # Generate descriptive text
  h_desc <- switch(h_pos,
                   "1" = if (structure$n_h_regions == 1) "Full" else "Top",
                   "2" = if (structure$n_h_regions == 2) "Bottom" else "Middle",
                   "3" = "Bottom",
                   paste0("Region ", h_pos))

  v_desc <- switch(v_pos,
                   "1" = if (structure$n_v_regions == 1) "" else "Left",
                   "2" = if (structure$n_v_regions == 2) "Right" else "Middle",
                   "3" = "Right",
                   paste0("Region ", v_pos))

  paste(h_desc, v_desc)
})

refresh_dotplot_with_regions <- function(active_region_configs) {
  req(dotplot_state$plot_ready, data_in(), data_def_in())

  if (restored_cache_authoritative()) {
    dotplot_debug_log("Skipping region refresh while restored cache is authoritative", 1)
    return(invisible(NULL))
  }

  # Keep labels synced to UI values
  dotplot_state$axis_config$x_label <- input$x_axis_label
  dotplot_state$axis_config$y_label <- input$y_axis_label

  tryCatch({
    current_plot_params <- dotplot_extract_plot_parameters(input)

    plot_result <- dotplot_create_complete_plot_with_regions_enhanced(
      data = data_in(),
      data_def = data_def_in(),
      axis_config = dotplot_state$axis_config,
      thresholds = dotplot_state$thresholds,
      color_rules = dotplot_state$color_rules,
      plot_params = current_plot_params,
      region_configs = active_region_configs,
      region_structure = region_structure(),
      ui_labels = list(
        x_label = input$x_axis_label,
        y_label = input$y_axis_label
      )
    )

    if (!is.null(plot_result)) {
      dotplot_state$current_plot <- plot_result
      dotplot_state$base_plot_without_labels <- plot_result
      dotplot_state$plot_request <- current_plot_params
      dotplot_state$plot_ui_cache <- dotplot_flatten_plot_ui_cache_for_restore(
        dotplot_capture_plot_ui_cache(input, dotplot_state, region_configs, region_structure)
      )
      dotplot_debug_log("Region style refresh completed", 2)
    } else {
      dotplot_debug_log("Region style refresh returned NULL plot", 1)
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Region style refresh error:", e$message), 1)

    # Fallback: standard plot without regions
    tryCatch({
      plot_result <- dotplot_create_complete_plot_enhanced(
        data = data_in(),
        data_def = data_def_in(),
        axis_config = dotplot_state$axis_config,
        thresholds = dotplot_state$thresholds,
        color_rules = dotplot_state$color_rules,
        plot_params = extracted_params,
        region_configs = region_configs(),
        region_structure = region_structure(),
        ui_labels = current_ui_labels
      )

      if (!is.null(plot_result)) {
        dotplot_state$current_plot <- plot_result
        dotplot_state$base_plot_without_labels <- plot_result
        dotplot_debug_log("Fallback plot created successfully", 1)
      }
    }, error = function(e2) {
      dotplot_debug_log(paste("Fallback also failed:", e2$message), 1)
    })
  })
}

observeEvent(input$reset_region_styling, {
  region_configs(list())

  colourpicker::updateColourInput(session, "region_point_color", value = "#E0E0E0")
  updateNumericInput(session, "region_point_size", value = 1)
  updateNumericInput(session, "region_point_alpha", value = 0.7)
  updateSelectInput(session, "region_point_shape", selected = 19)

  if (isTRUE(dotplot_state$plot_ready)) {
    refresh_dotplot_with_regions(region_configs())
  }

  showNotification("All region styling has been reset.", type = "message", duration = 2)
  dotplot_debug_log("Reset all region styling to defaults", 1)
}, ignoreInit = TRUE)


observeEvent(c(input$region_point_color,
               input$region_point_size,
               input$region_point_alpha,
               input$region_point_shape), {

                 req(selected_region())

                 # During session restore the reactive store -- not the
                 # widgets -- is the source of truth for per-region styling.
                 # update_region_inputs() (and sync_dotplot_ui_from_state())
                 # echo the restored colour/size/alpha/shape back into the
                 # widgets; the resulting input change fires this observer.
                 # If we did nothing to stop it, the observer would
                 #   1. overwrite region_configs[[selected_region()]] with
                 #      a composite built from whichever input values have
                 #      round-tripped so far (often partially stale, e.g.
                 #      correct colour but factory-default size), and
                 #   2. call refresh_dotplot_with_regions() which
                 #      regenerates the plot -- wiping out the ggplot
                 #      object just restored from the session snapshot and
                 #      collapsing per-region colouring to whichever
                 #      region happened to dominate the regeneration
                 #      inputs.  Short-circuit while the restore guard is
                 #      raised.
                 if (isTRUE(isolate(dotplot_state$restore_in_progress)) || restored_cache_authoritative()) {
                   dotplot_debug_log(
                     "Ignoring region_point_* input change during session restore", 2)
                   return()
                 }

                 # Persist config immediately so it also applies to the first generated plot.
                 configs <- region_configs()
                 region_key <- selected_region()

                 config <- list(
                   color = input$region_point_color,
                   size = input$region_point_size,
                   alpha = input$region_point_alpha,
                   shape = as.numeric(input$region_point_shape)
                 )

                 # Idempotence guard: when update_region_inputs() merely
                 # echoes the stored style back to the widgets (either
                 # during restore-after-guard-clear or after a plain
                 # region button click), skip both the write and the
                 # refresh.  This prevents redundant plot regeneration
                 # cycles from round-tripping through the reactive graph.
                 existing <- configs[[region_key]]
                 if (!is.null(existing) && identical(existing, config)) {
                   return()
                 }

                 configs[[region_key]] <- config
                 region_configs(configs)

                 update_region_button_after_config(session, ns, region_key, config, region_structure())

                 if (isTRUE(dotplot_state$plot_ready) && !is.null(data_in()) && !is.null(data_def_in())) {
                   dotplot_debug_log("Executing region style refresh with current parameters", 2)
                   current_region_configs <- isolate(region_configs())
                   refresh_dotplot_with_regions(current_region_configs)
                 } else {
                   dotplot_debug_log("Stored region style before first plot generation", 2)
                 }

               }, ignoreInit = TRUE)


invisible(NULL)
}

# ------------------------------------------------------------------------------
# create_region_button_matrix
# Purpose: Builds a button matrix representing currently available plot regions.
# Structure:
#   - Section 1: Build rows/columns based on computed region structure.
#   - Section 2: Apply current style preview to each region button.
# Parameters:
#   - structure: [list] - Region matrix metadata.
#   - ns: [function] - Namespace helper.
#   - region_configs: [list] - Region style configurations.
# Returns: tagList containing matrix button UI.
# ------------------------------------------------------------------------------
create_region_button_matrix <- function(structure, ns, region_configs = list()) {
  rows <- list()
  for (h in 1:structure$n_h_regions) {
    buttons <- list()
    for (v in 1:structure$n_v_regions) {
      button_id <- paste0("region_", h, "_", v)
      region_key <- paste0(h, "_", v)
      button_label <- create_region_button_label(h, v, structure)
      config <- region_configs[[region_key]]
      is_configured <- !is.null(config)
      if (!is_configured) config <- list(color = "#E0E0E0", size = 1, alpha = 1, shape = 19)
      color_dot <- sprintf("<span style='display: inline-block; width: 8px; height: 8px; background-color: %s; border-radius: 50%%; margin-right: 6px; opacity: %s; border: 1px solid rgba(0,0,0,0.2);'></span>", config$color, ifelse(is_configured, config$alpha, 0.3))
      button_style <- sprintf("margin: 2px; width: %dpx; height: %dpx; font-size: %dpx; border: 2px solid %s; transition: all 0.2s ease;", max(70, 300 / structure$n_v_regions), max(35, 120 / structure$n_h_regions), max(10, 14 - structure$total_regions), ifelse(is_configured, config$color, "#dee2e6"))
      button_class <- ifelse(is_configured, "btn btn-outline-primary", "btn btn-outline-secondary")
      buttons[[v]] <- actionButton(ns(button_id), HTML(paste0(color_dot, button_label)), style = button_style, class = button_class)
    }
    rows[[h]] <- div(style = "margin: 2px 0;", do.call(tagList, buttons))
  }
  do.call(tagList, rows)
}

# ------------------------------------------------------------------------------
# remap_region_configs_by_overlap
# Purpose: Transfers existing region styles to a changed region matrix by using
#   geometric overlap, so split regions inherit styling from their parent area.
# Parameters:
#   - old_configs: [list] - Existing region style settings keyed by old IDs.
#   - old_structure: [list] - Previous region matrix metadata.
#   - new_structure: [list] - Updated region matrix metadata.
# Returns: List of remapped region style settings.
# ------------------------------------------------------------------------------
remap_region_configs_by_overlap <- function(old_configs, old_structure, new_structure) {
  if (length(old_configs) == 0 || is.null(old_structure) || is.null(new_structure)) {
    return(old_configs)
  }

  old_bounds <- dotplot_region_bounds(old_structure)
  new_bounds <- dotplot_region_bounds(new_structure)

  old_keys <- intersect(names(old_configs), names(old_bounds))
  if (length(old_keys) == 0) {
    return(list())
  }

  x_thresholds <- c(old_structure$v_thresholds, new_structure$v_thresholds)
  y_thresholds <- c(old_structure$h_thresholds, new_structure$h_thresholds)

  x_thresholds <- as.numeric(x_thresholds[is.finite(x_thresholds)])
  y_thresholds <- as.numeric(y_thresholds[is.finite(y_thresholds)])

  x_pad <- if (length(x_thresholds) > 1) diff(range(x_thresholds)) * 0.5 + 1 else 1
  y_pad <- if (length(y_thresholds) > 1) diff(range(y_thresholds)) * 0.5 + 1 else 1

  x_limits <- if (length(x_thresholds) > 0) c(min(x_thresholds) - x_pad, max(x_thresholds) + x_pad) else c(-1, 1)
  y_limits <- if (length(y_thresholds) > 0) c(min(y_thresholds) - y_pad, max(y_thresholds) + y_pad) else c(-1, 1)

  clamp_bounds <- function(bounds) {
    lapply(bounds, function(region) {
      region$x_min <- if (is.finite(region$x_min)) region$x_min else x_limits[1]
      region$x_max <- if (is.finite(region$x_max)) region$x_max else x_limits[2]
      region$y_min <- if (is.finite(region$y_min)) region$y_min else y_limits[1]
      region$y_max <- if (is.finite(region$y_max)) region$y_max else y_limits[2]
      region
    })
  }

  old_bounds <- clamp_bounds(old_bounds)
  new_bounds <- clamp_bounds(new_bounds)

  migrated_configs <- list()

  for (new_key in names(new_bounds)) {
    new_region <- new_bounds[[new_key]]
    best_old_key <- NULL
    best_overlap <- -Inf

    for (old_key in old_keys) {
      old_region <- old_bounds[[old_key]]

      x_overlap <- min(new_region$x_max, old_region$x_max) - max(new_region$x_min, old_region$x_min)
      y_overlap <- min(new_region$y_max, old_region$y_max) - max(new_region$y_min, old_region$y_min)
      overlap_area <- max(0, x_overlap) * max(0, y_overlap)

      if (overlap_area > best_overlap) {
        best_overlap <- overlap_area
        best_old_key <- old_key
      }
    }

    if (!is.null(best_old_key) && best_overlap > 0) {
      migrated_configs[[new_key]] <- old_configs[[best_old_key]]
    }
  }

  migrated_configs
}

# ------------------------------------------------------------------------------
# dotplot_region_bounds
# Purpose: Calculates region rectangle boundaries from threshold metadata.
# Parameters:
#   - structure: [list] - Region matrix metadata.
# Returns: Named list of bounds per region with x/y min and max values.
# ------------------------------------------------------------------------------
dotplot_region_bounds <- function(structure) {
  v_boundaries <- c(-Inf, structure$v_thresholds, Inf)
  h_boundaries <- c(-Inf, structure$h_thresholds, Inf)
  bounds <- list()

  for (h in seq_len(structure$n_h_regions)) {
    y_raw_idx <- structure$n_h_regions + 1 - h
    for (v in seq_len(structure$n_v_regions)) {
      region_key <- paste0(h, "_", v)
      bounds[[region_key]] <- list(
        x_min = v_boundaries[v],
        x_max = v_boundaries[v + 1],
        y_min = h_boundaries[y_raw_idx],
        y_max = h_boundaries[y_raw_idx + 1]
      )
    }
  }

  bounds
}

# ------------------------------------------------------------------------------
# create_region_button_label
# Purpose: Returns the display label for a region button.
# Structure:
#   - Section 1: Build horizontal descriptor.
#   - Section 2: Build vertical descriptor and concatenate.
# Parameters:
#   - h_pos: [numeric] - Horizontal region index.
#   - v_pos: [numeric] - Vertical region index.
#   - structure: [list] - Region matrix metadata.
# Returns: Character label for the region button.
# ------------------------------------------------------------------------------
create_region_button_label <- function(h_pos, v_pos, structure) {
  h_desc <- switch(as.character(h_pos),
    "1" = if (structure$n_h_regions == 1) "All" else "Top",
    "2" = if (structure$n_h_regions == 2) "Bottom" else "Middle",
    "3" = "Bottom",
    paste0("H", h_pos)
  )
  v_desc <- switch(as.character(v_pos),
    "1" = if (structure$n_v_regions == 1) "" else "Left",
    "2" = if (structure$n_v_regions == 2) "Right" else "Middle",
    "3" = "Right",
    paste0("V", v_pos)
  )
  trimws(paste(h_desc, v_desc))
}

# ------------------------------------------------------------------------------
# update_region_button_after_config
# Purpose: Updates region button styles after applying custom styling.
# Structure:
#   - Section 1: Resolve region coordinates.
#   - Section 2: Update button preview color, opacity, and border.
# Parameters:
#   - session: [shinysession] - Active module session.
#   - ns: [function] - Namespace helper.
#   - region_key: [character] - Region identifier in row_col format.
#   - config: [list] - Region style settings.
#   - structure: [list] - Region matrix metadata.
# Returns: Invisible NULL.
# ------------------------------------------------------------------------------
update_region_button_after_config <- function(session, ns, region_key, config, structure) {
  coords <- strsplit(region_key, "_")[[1]]
  h <- as.numeric(coords[1]); v <- as.numeric(coords[2])
  button_id <- paste0(ns(""), "region_", h, "_", v)
  shinyjs::runjs(sprintf("$('#%s span').css({'background-color': '%s','opacity': '%s'});", button_id, config$color, config$alpha))
  shinyjs::runjs(sprintf("$('#%s').css('border-color', '%s');", button_id, config$color))
  shinyjs::runjs(sprintf("$('#%s').removeClass('btn-outline-secondary').addClass('btn-outline-primary');", button_id))
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# reset_region_button_after_reset
# Purpose: Resets a region button to default styling preview.
# Structure:
#   - Section 1: Resolve region coordinates.
#   - Section 2: Restore default button class and dot preview.
# Parameters:
#   - session: [shinysession] - Active module session.
#   - ns: [function] - Namespace helper.
#   - region_key: [character] - Region identifier in row_col format.
#   - structure: [list] - Region matrix metadata.
# Returns: Invisible NULL.
# ------------------------------------------------------------------------------
reset_region_button_after_reset <- function(session, ns, region_key, structure) {
  coords <- strsplit(region_key, "_")[[1]]
  h <- as.numeric(coords[1]); v <- as.numeric(coords[2])
  button_id <- paste0(ns(""), "region_", h, "_", v)
  shinyjs::runjs(sprintf("$('#%s').css('border-color', '#dee2e6').removeClass('btn-outline-primary').addClass('btn-outline-secondary'); $('#%s span').css({'background-color': '#6c757d','opacity': '0.3'});", button_id, button_id))
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# add_region_hover_effects
# Purpose: Adds JavaScript hover effects to region matrix buttons.
# Structure:
#   - Section 1: Inject hover in/out JS handlers for region button IDs.
# Parameters:
#   - session: [shinysession] - Active module session.
#   - ns: [function] - Namespace helper.
#   - structure: [list] - Region matrix metadata.
# Returns: Invisible NULL.
# ------------------------------------------------------------------------------
add_region_hover_effects <- function(session, ns, structure) {
  shinyjs::runjs(sprintf("$(document).ready(function(){ $('[id^=\"%sregion_\"]').hover(function(){ $(this).css({'box-shadow':'0 0 15px rgba(52, 152, 219, 0.4)','transform':'scale(1.05)','z-index':'10'});}, function(){ $(this).css({'box-shadow':'none','transform':'scale(1)','z-index':'auto'});});});", ns("")))
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# dotplot_assign_points_to_regions
# Purpose: Assigns each point to a region based on threshold boundaries.
# Structure:
#   - Section 1: Build vertical and horizontal boundaries from thresholds.
#   - Section 2: Assign region indices and compose region IDs.
# Parameters:
#   - plot_data: [data.frame] - Plot data with x and y columns.
#   - region_structure: [list] - Region matrix metadata.
# Returns: Plot data with region_h, region_v, and region_id columns.
# ------------------------------------------------------------------------------
dotplot_assign_points_to_regions <- function(plot_data, region_structure) {
  if (is.null(region_structure)) { plot_data$region_id <- "1_1"; return(plot_data) }
  v_boundaries <- c(-Inf, region_structure$v_thresholds, Inf)
  h_boundaries <- c(-Inf, region_structure$h_thresholds, Inf)
  region_h_raw <- cut(plot_data$y, breaks = h_boundaries, labels = FALSE, include.lowest = TRUE)
  plot_data$region_v <- cut(plot_data$x, breaks = v_boundaries, labels = FALSE, include.lowest = TRUE)
  max_h_region <- region_structure$n_h_regions
  plot_data$region_h <- max_h_region + 1 - region_h_raw
  plot_data$region_id <- paste0(plot_data$region_h, "_", plot_data$region_v)
  plot_data
}

# ------------------------------------------------------------------------------
# dotplot_apply_region_styling_fixed
# Purpose: Applies region-specific point styling to plotted data.
# Structure:
#   - Section 1: Initialize default style vectors.
#   - Section 2: Override styles for configured regions.
# Parameters:
#   - plot_data: [data.frame] - Plot data with region identifiers.
#   - region_configs: [list] - Region style settings keyed by region ID.
#   - default_style: [list] - Fallback styling values.
# Returns: Plot data with color, size, alpha, and shape style columns.
# ------------------------------------------------------------------------------
dotplot_apply_region_styling_fixed <- function(plot_data, region_configs, default_style = NULL) {
  if (is.null(default_style)) default_style <- list(color = "#E0E0E0", size = 1, alpha = 1, shape = 19)
  n_points <- nrow(plot_data)
  plot_data$color <- rep(default_style$color, n_points)
  plot_data$size <- rep(default_style$size, n_points)
  plot_data$alpha <- rep(default_style$alpha, n_points)
  plot_data$shape <- rep(default_style$shape, n_points)
  for (region_id in names(region_configs)) {
    mask <- plot_data$region_id == region_id
    config <- region_configs[[region_id]]
    if (sum(mask, na.rm = TRUE) > 0) {
      plot_data$color[mask] <- config$color
      plot_data$size[mask] <- config$size
      plot_data$alpha[mask] <- config$alpha
      plot_data$shape[mask] <- config$shape
    }
  }
  plot_data
}
