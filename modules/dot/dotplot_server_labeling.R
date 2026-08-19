# ==============================================================================
# dotplot_server_labeling.R - Dotplot labeling observers
#
# Purpose: Hosts enhanced label styling UI and observers for per-protein and
# master label/dot settings in the Dotplot module.
#
# Structure:
#   - Enhanced selected proteins UI rendering
#   - Individual label control observer wiring
#   - Apply/master/reset/clear labeling handlers
#
# Dependencies: shiny, colourpicker
# Called by: modDotPlotServer()
# ==============================================================================

# ------------------------------------------------------------------------------
# dotplot_init_labeling_observers
# Purpose: Initializes Dotplot labeling observers and related UI outputs.
# Structure:
#   - Section 1: Render per-protein label controls and dynamic remove buttons.
#   - Section 2: Apply labeling settings and synchronize master controls.
#   - Section 3: Reset and clear label/selection states.
# Parameters:
#   - input/output/session/ns/dotplot_debug_log: [various] - Standard module dependencies.
#   - rv: [reactivevalues] - Shared app state.
#   - dotplot_state: [reactivevalues] - Dotplot state container.
#   - selected_protein_vector_dot: [reactiveVal] - Selected protein identifiers.
#   - selected_data_dot: [reactiveVal] - Selected protein data table.
#   - protein_label_settings_dot: [reactiveVal] - Per-protein label style settings.
#   - dot_protein_labels: [reactiveVal] - Label cache for dot plots.
# Returns: Invisible NULL.
# ------------------------------------------------------------------------------
dotplot_init_labeling_observers <- function(input, output, session, ns, dotplot_debug_log, rv, dotplot_state,
                                            selected_protein_vector_dot, selected_data_dot,
                                            protein_label_settings_dot, dot_protein_labels) {
output$enhanced_selectedProteins_dot <- renderUI({
  tryCatch({
    selected_proteins <- selected_protein_vector_dot()

    if (is.null(selected_proteins) || length(selected_proteins) == 0) {
      return(div(
        style = "padding: 15px; border: 1px solid #ddd; border-radius: 5px; background-color: #f8f9fa; min-height: 120px;",
        p("No proteins selected", style = "color: #666; margin: 0; text-align: center; padding-top: 30px;")
      ))
    }

    dotplot_debug_log(paste("Generating enhanced UI for", length(selected_proteins), "proteins in dot module"), 2)

    # Get current settings or initialize
    current_settings <- protein_label_settings_dot()

    # Get default dot colors for each protein
    default_dot_colors <- get_default_dot_colors_for_proteins_dot(selected_proteins)

    # Create UI elements for each protein
    protein_rows <- lapply(seq_along(selected_proteins), function(i) {
      protein <- selected_proteins[i]

      # Get existing settings or create defaults
      existing_row <- current_settings[current_settings$protein_id == protein, ]

      if (nrow(existing_row) > 0) {
        label_color <- existing_row$label_color[1]
        dot_color <- existing_row$dot_color[1]
        use_custom <- existing_row$use_custom_dot_color[1]
      } else {
        label_color <- "#000000"  # Default black
        dot_color <- default_dot_colors[i]
        use_custom <- FALSE
      }

      # Create row UI with proper spacing
      fluidRow(
        style = "border-bottom: 1px solid #eee; padding: 12px 5px; margin: 0px;",
        column(width = 3,
               div(style = "padding-top: 10px;",
                   strong(substr(protein, 1, 16))
               )
        ),
        column(width = 3,
               div(style = "padding: 2px;",
                   colourInput(
                     ns(paste0("labelColor_dot_", i)),
                     "Label:",
                     value = label_color
                   )
               )
        ),
        column(width = 3,
               div(style = "padding: 2px;",
                   colourInput(
                     ns(paste0("dotColor_dot_", i)),
                     "Dot:",
                     value = dot_color
                   )
               )
        ),
        column(width = 2,
               div(style = "padding-top: 15px;",
                   checkboxInput(
                     ns(paste0("useDotColor_dot_", i)),
                     "Custom Dot Color",
                     value = use_custom,
                     width = "100%"
                   )
               )
        ),
        column(width = 1,
               div(style = "padding-top: 15px; text-align: center;",
                   tags$button(
                     class = "btn btn-danger btn-xs",
                     style = "padding: 1px 6px; font-size: 11px; line-height: 1.4;",
                     onclick = sprintf(
                       "Shiny.setInputValue(\"%s\", \"%s\", {priority: \"event\"});",
                       ns("remove_protein_click_dot"),
                       gsub('"', '\\\\"', protein, fixed = TRUE)
                     ),
                     icon("times")
                   )
               )
        )
      )
    })

    # Return the enhanced list
    tagList(
      # Header row
      fluidRow(
        style = "background-color: #f8f9fa; padding: 12px; margin: 0px; border: 1px solid #ddd; border-bottom: none; font-weight: bold;",
        column(width = 3, "Protein"),
        column(width = 3, "Label Color"),
        column(width = 3, "Dot Color"),
        column(width = 2, "Custom Dot"),
        column(width = 1, "Remove")
      ),
      # Protein rows container
      div(
        style = "border: 1px solid #ddd; border-top: none; padding: 8px; max-height: 400px; min-height: 200px; overflow-y: auto;",
        protein_rows
      )
    )

  }, error = function(e) {
    dotplot_debug_log(paste("Error generating enhanced UI for dot module:", e$message), 1)
    return(div("Error generating protein controls"))
  })
})

# ========================================
# Individual Protein Removal Observer
# ========================================

# Remove a single protein via its per-row button in the Selected Proteins list
observeEvent(input$remove_protein_click_dot, {
  tryCatch({
    protein_to_remove <- input$remove_protein_click_dot
    if (is.null(protein_to_remove) || !nzchar(protein_to_remove)) return()

    dotplot_debug_log(paste("Removing protein from dot plot:", protein_to_remove), 1)

    current_data <- selected_data_dot()
    if (is.null(current_data) || !is.data.frame(current_data) || nrow(current_data) == 0) return()

    selected_identifier <- input$GeneIdentifierColumn_dot
    if (is.null(selected_identifier) || !nzchar(selected_identifier) ||
        !(selected_identifier %in% colnames(current_data))) return()

    filtered_data <- current_data[current_data[[selected_identifier]] != protein_to_remove, , drop = FALSE]
    selected_data_dot(filtered_data)

    if (nrow(filtered_data) > 0) {
      selected_protein_vector_dot(unique(as.character(filtered_data[[selected_identifier]])))
    } else {
      selected_protein_vector_dot(character())
    }

    current_settings <- protein_label_settings_dot()
    if (nrow(current_settings) > 0) {
      protein_label_settings_dot(current_settings[current_settings$protein_id != protein_to_remove, ])
    }

    dotplot_debug_log(paste("Successfully removed protein from dot plot:", protein_to_remove), 1)
    showNotification(paste("Removed", protein_to_remove, "from selection"),
                     type = "message", duration = 2)
  }, error = function(e) {
    dotplot_debug_log(paste("Error removing protein from dot plot:", e$message), 1)
  })
})


observeEvent(input$applySettings_dot, {
  tryCatch({
    req(rv$data_mod, rv$data_def)

    dotplot_debug_log("Applying dot plot label settings", 1)

    selected_proteins <- selected_protein_vector_dot()

    if (is.null(selected_proteins) || length(selected_proteins) == 0) {
      showNotification("No proteins to configure", type = "warning")
      return()
    }

    # Check if plot is available
    if (is.null(dotplot_state$current_plot)) {
      showNotification("No dot plot available. Please generate a plot first.", type = "warning")
      return()
    }

    # SOLUTION 1: Store original plot without labels if not already stored
    if (is.null(dotplot_state$base_plot_without_labels)) {
      dotplot_debug_log("Storing original plot without labels for clean labeling", 1)
      dotplot_state$base_plot_without_labels <- dotplot_state$current_plot
    }

    # SOLUTION 2: Always start from the clean base plot without labels
    clean_base_plot <- dotplot_state$base_plot_without_labels

    # Clear existing labels
    dot_protein_labels(data.frame())

    # Collect settings
    new_settings <- data.frame(
      protein_id = selected_proteins,
      label_color = character(length(selected_proteins)),
      dot_color = character(length(selected_proteins)),
      use_custom_dot_color = logical(length(selected_proteins)),
      stringsAsFactors = FALSE
    )

    for (i in seq_along(selected_proteins)) {
      label_input_id <- paste0("labelColor_dot_", i)
      dot_input_id <- paste0("dotColor_dot_", i)
      checkbox_input_id <- paste0("useDotColor_dot_", i)

      new_settings$label_color[i] <- input[[label_input_id]] %||% "#000000"
      new_settings$dot_color[i] <- input[[dot_input_id]] %||% "#E0E0E0"
      new_settings$use_custom_dot_color[i] <- input[[checkbox_input_id]] %||% FALSE
    }

    protein_label_settings_dot(new_settings)

    # Create label data with DEBUG version
    new_label_data <- create_dot_label_data_enhanced_FIXED(
      selected_proteins, rv, input, dotplot_state, dotplot_debug_log, new_settings
    )

    if (is.null(new_label_data) || nrow(new_label_data) == 0) {
      showNotification("Could not create label data for selected proteins", type = "error")
      return()
    }

    # Store labels
    dot_protein_labels(new_label_data)
    dotplot_state$labels <- list(
      manual_labels = new_label_data,
      protein_label_settings = new_settings,
      selected_proteins = selected_proteins
    )

    # SOLUTION 3: Apply labels to the CLEAN base plot (not the current plot with old labels)
    updated_plot <- apply_labels_to_dot_plot_enhanced_FIXED(clean_base_plot, new_label_data, input, dotplot_debug_log)

    # Store updated plot
    dotplot_state$current_plot <- updated_plot

    dotplot_debug_log(paste("Successfully applied fresh labels to", length(selected_proteins), "proteins"), 1)
    showNotification(paste("Applied settings and labeled", length(selected_proteins), "proteins"),
                     type = "message", duration = 3)

  }, error = function(e) {
    dotplot_debug_log(paste("ERROR in applySettings_dot:", e$message), 1)
    showNotification(paste("Error applying settings:", e$message), type = "error")
  })
})

# ========================================
# Master Controls Observers
# ========================================

# Master Label Color Observer
observeEvent(input$masterLabelColor_dot, {
  tryCatch({
    selected_proteins <- selected_protein_vector_dot()
    master_color <- input$masterLabelColor_dot

    if (is.null(selected_proteins) || length(selected_proteins) == 0 || is.null(master_color)) {
      return()
    }

    dotplot_debug_log(paste("Updating all label colors to master color:", master_color), 1)

    # Update all individual label color inputs
    for (i in seq_along(selected_proteins)) {
      label_input_id <- paste0("labelColor_dot_", i)
      updateColourInput(session, label_input_id, value = master_color)
    }

    dotplot_debug_log(paste("Updated", length(selected_proteins), "label colors"), 2)

  }, error = function(e) {
    dotplot_debug_log(paste("Error updating master label color:", e$message), 1)
  })
})

# Master Dot Color Observer
observeEvent(input$masterDotColor_dot, {
  tryCatch({
    selected_proteins <- selected_protein_vector_dot()
    master_color <- input$masterDotColor_dot

    if (is.null(selected_proteins) || length(selected_proteins) == 0 || is.null(master_color)) {
      return()
    }

    dotplot_debug_log(paste("Updating all dot colors to master color:", master_color), 1)

    # Update all individual dot color inputs
    for (i in seq_along(selected_proteins)) {
      dot_input_id <- paste0("dotColor_dot_", i)
      updateColourInput(session, dot_input_id, value = master_color)
    }

    dotplot_debug_log(paste("Updated", length(selected_proteins), "dot colors"), 2)

  }, error = function(e) {
    dotplot_debug_log(paste("Error updating master dot color:", e$message), 1)
  })
})

# Master Custom Dot Color Checkbox Observer
observeEvent(input$masterCustomDot_dot, {
  tryCatch({
    selected_proteins <- selected_protein_vector_dot()
    master_enabled <- input$masterCustomDot_dot

    if (is.null(selected_proteins) || length(selected_proteins) == 0 || is.null(master_enabled)) {
      return()
    }

    dotplot_debug_log(paste("Setting all custom dot color checkboxes to:", master_enabled), 1)

    # Update all individual custom dot color checkboxes
    for (i in seq_along(selected_proteins)) {
      checkbox_input_id <- paste0("useDotColor_dot_", i)
      updateCheckboxInput(session, checkbox_input_id, value = master_enabled)
    }

    dotplot_debug_log(paste("Updated", length(selected_proteins), "custom dot color checkboxes"), 2)

  }, error = function(e) {
    dotplot_debug_log(paste("Error updating master custom dot checkbox:", e$message), 1)
  })
})

# ========================================
# Reset Colors Observer - Labeling
# ========================================

observeEvent(input$resetColors_dot, {
  tryCatch({
    selected_proteins <- selected_protein_vector_dot()

    if (is.null(selected_proteins) || length(selected_proteins) == 0) {
      return()
    }

    # Reset to default settings
    protein_label_settings_dot(data.frame(
      protein_id = character(),
      label_color = character(),
      dot_color = character(),
      use_custom_dot_color = logical(),
      stringsAsFactors = FALSE
    ))

    dotplot_debug_log("Reset protein color settings for dot plot", 1)
    showNotification("Reset protein color settings to defaults", type = "message", duration = 3)

  }, error = function(e) {
    dotplot_debug_log(paste("Error resetting dot plot settings:", e$message), 1)
  })
})

# ========================================
# Clear All Labels Observer - Labeling
# ========================================

observeEvent(input$clearLabels_dot, {
  tryCatch({
    # Clear labels
    dot_protein_labels(data.frame())
    dotplot_state$labels <- list(
      manual_labels = data.frame(),
      protein_label_settings = protein_label_settings_dot(),
      selected_proteins = selected_protein_vector_dot()
    )

    # SOLUTION 4: Restore the original plot without labels
    if (!is.null(dotplot_state$base_plot_without_labels)) {
      dotplot_state$current_plot <- dotplot_state$base_plot_without_labels
      dotplot_debug_log("Restored original plot without labels", 1)
      showNotification("Cleared all labels from dot plot", type = "message", duration = 3)
    } else {
      dotplot_debug_log("No base plot stored - cannot clear labels cleanly", 1)
      showNotification("Cannot clear labels - no base plot available", type = "warning")
    }

  }, error = function(e) {
    dotplot_debug_log(paste("Error clearing dot labels:", e$message), 1)
  })
})

# ========================================
# Clear Selection Observer - Labeling
# ========================================

observeEvent(input$clearSelection_dot, {
  tryCatch({
    # Clear all selected proteins and data
    selected_data_dot(NULL)
    selected_protein_vector_dot(character())

    # Also clear protein label settings
    protein_label_settings_dot(data.frame(
      protein_id = character(),
      label_color = character(),
      dot_color = character(),
      use_custom_dot_color = logical(),
      stringsAsFactors = FALSE
    ))

    dotplot_debug_log("Cleared all selected proteins and settings for dot plot", 1)
    showNotification("Cleared protein selection and settings", type = "message", duration = 3)

  }, error = function(e) {
    dotplot_debug_log(paste("Error clearing selection in dot plot:", e$message), 1)
  })
})

  invisible(NULL)
}
