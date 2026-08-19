# ==============================================================================
# File: modules/Data Wizard/basemean/datawizard_basemean_observer.R
#
# Purpose:
#   Contains all observe() and observeEvent() blocks for the Basemean
#   submodule of the Data Wizard. This file centralizes reactive side-effects
#   (UI dropdown updates, data mutation on button clicks, UI config import)
#   so that the orchestrator file stays lean and focused on wiring.
#
# Architectural Role:
#   Observer layer of the basemean module. Called from modBasemeanServer() via
#   register_basemean_observers() after state and the apply_ui_config helper
#   are initialized. All observers run inside the moduleServer() closure of
#   the orchestrator. Pure logic is delegated to functions from
#   datawizard_basemean_logic.R. Reactive state comes from
#   datawizard_basemean_state.R via the `state` list argument.
#
# Structure:
#   1. register_basemean_observers() - Registration function containing:
#      a. Abundance type dropdown observer
#      b. Sample dropdown observer (depends on abundance type)
#      c. Add Basemean button observer
#      d. Clear Basemeans button observer
#      e. UI config import observer
#      f. UI config write-back observer
#
# Notes for future developers:
#   - Observer registration order matters: the sample observer depends on
#     the abundance type observer having populated the type dropdown first.
#   - Every observer is wrapped in tryCatch for robustness.
#   - apply_ui_config is passed as a closure that captures state, input,
#     and session; do not call it from outside this module.
#   - ui_config_update_active() is a guard flag from the state file that
#     prevents import observers from re-entering during programmatic UI updates.
#   - The UI config write-back observer runs on every input change. It uses
#     isolate() to avoid creating a reactive dependency on the entire config
#     object, and only writes back for reactiveVal targets.
# ==============================================================================


#' Register all observers for the basemean module.
#'
#' @param input              Shiny input object from moduleServer closure.
#' @param output             Shiny output object from moduleServer closure.
#' @param session            Shiny session object.
#' @param ns                 Namespace function for this module.
#' @param state              Named list from create_basemean_state().
#' @param get_data           Function returning the current data frame.
#' @param set_data           Function to write back a modified data frame.
#' @param data_def           Reactive expression returning the metadata data frame.
#' @param UI_config          Reactive, reactiveVal, list, or NULL; UI configuration.
#' @param apply_ui_config    Function apply_ui_config_basemean(cfg) from orchestrator.
#' @param debug_log          Logging function with signature (message, level).
#' @param DEBUG_LEVEL        Numeric debug verbosity level.
register_basemean_observers <- function(input, output, session, ns,
                                        state, get_data, set_data, data_def,
                                        UI_config, apply_ui_config,
                                        debug_log, DEBUG_LEVEL,
                                        metadata_revision_debounced = reactive(NULL),
                                        primary_working_revision_debounced = reactive(NULL),
                                        metadata_assignment_pending = reactive(FALSE),
                                        metadata_meaningful_ready = reactive(FALSE),
                                        data_revision_signature = reactive(NULL)) {

  ui_config_update_active <- state$ui_config_update_active
  current_ui_config       <- state$current_ui_config
  get_ui_config           <- state$get_ui_config
  abundance_choice_cache <- reactiveVal(list(signature = NULL, choices = character(0)))
  sample_choice_cache <- reactiveVal(list(signature = NULL, choices = character(0)))

  format_revision_signature <- function(extra = NULL) {
    sig <- tryCatch(data_revision_signature(), error = function(e) NULL)
    paste(c(unlist(sig, use.names = TRUE), extra), collapse = "|")
  }

  # --------------------------------------------------------------------------
  # a. Abundance type dropdown observer
  #    Populates the abundance type selectInput from metadata whenever the
  #    metadata reactive changes. Skips update during active config imports
  #    to prevent feedback loops.
  # --------------------------------------------------------------------------

  refresh_basemean_choices <- function() {
    if (isTRUE(metadata_assignment_pending()) && !isTRUE(metadata_meaningful_ready())) {
      debug_log("Metadata assignment pending; deferring basemean abundance choices", 2)
      return()
    }
    loaded_data <- isolate(get_data())
    req(is.data.frame(loaded_data))
    meta <- isolate(data_def())
    req(meta)

    if (ui_config_update_active()) return()

    cache_signature <- format_revision_signature("abundance-types")
    cached_types <- abundance_choice_cache()
    if (!is.null(cached_types$signature) && identical(cached_types$signature, cache_signature)) {
      valid_types <- cached_types$choices
    } else {
      valid_types <- unique(meta$Content[
        meta$Content %in% c(
          "Raw Abundance", "Normalized Abundance",
          "Imputed Raw Abundance", "Imputed Normalized Abundance", "Imputed Batch Corrected Normalized Abundance", "Imputed Batch Corrected Raw Abundance",
          "Batch Corrected Abundance", "Imputed Batch Corrected Abundance",
          "Batch Corrected Normalized Abundance",
          "Batch Corrected Raw Abundance"
        )
      ])
      abundance_choice_cache(list(signature = cache_signature, choices = valid_types))
    }

    debug_log(paste(
      "Updating abundance type dropdown — found", length(valid_types), "types"
    ), 1)

    current_selection <- isolate(input$abundance_type_basemean)
    if (is.null(current_selection) || !(current_selection %in% valid_types)) {
      current_selection <- valid_types[1]
    }

    updateSelectInput(
      session,
      "abundance_type_basemean",
      choices  = valid_types,
      selected = current_selection
    )
  }

  observeEvent(list(metadata_revision_debounced(), primary_working_revision_debounced()), {
    refresh_basemean_choices()
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # b. Sample dropdown observer
  #    Refreshes the sample selectizeInput whenever the abundance type or
  #    metadata changes. Applies a deferred sample selection if one was
  #    scheduled by a config import.
  # --------------------------------------------------------------------------

  observeEvent({
    list(input$abundance_type_basemean, metadata_revision_debounced(), primary_working_revision_debounced())
  }, {
    if (isTRUE(metadata_assignment_pending()) && !isTRUE(metadata_meaningful_ready())) {
      debug_log("Metadata assignment pending; deferring basemean sample choices", 2)
      return()
    }
    loaded_data <- isolate(get_data())
    req(is.data.frame(loaded_data))
    meta <- isolate(data_def())
    req(meta, input$abundance_type_basemean)

    if (ui_config_update_active()) return()

    cache_signature <- format_revision_signature(paste("samples", input$abundance_type_basemean, sep = "="))
    cached_samples <- sample_choice_cache()
    if (!is.null(cached_samples$signature) && identical(cached_samples$signature, cache_signature)) {
      valid_samples <- cached_samples$choices
    } else {
      # Preserve metadata/column order.  Lexicographic sorting places sample
      # 10 before sample 2 and made Basemean inconsistent with every other
      # Data Wizard module.
      valid_samples <- unique(na.omit(
        meta$Sample[meta$Content == input$abundance_type_basemean]
      ))
      sample_choice_cache(list(signature = cache_signature, choices = valid_samples))
    }

    current_samples <- isolate(input$sample_selection_basemean)
    valid_selected  <- intersect(current_samples, valid_samples)

    updateSelectizeInput(
      session,
      "sample_selection_basemean",
      choices  = valid_samples,
      selected = valid_selected
    )

    debug_log(paste(
      "Sample dropdown updated for type", input$abundance_type_basemean,
      "—", length(valid_samples), "samples available"
    ), 2)

    # Apply a deferred sample selection scheduled by a config import.
    if (!is.null(session$userData$apply_basemean_after_sample_update)) {
      cfg <- session$userData$apply_basemean_after_sample_update
      session$userData$apply_basemean_after_sample_update <- NULL

      if (!is.null(cfg$samples) && length(cfg$samples) > 0) {
        debug_log("Applying deferred sample selection from imported config", 1)
        isolate({
          updateSelectizeInput(
            session,
            "sample_selection_basemean",
            selected = cfg$samples
          )
        })
      }
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # c. Add Basemean button observer
  #    Validates inputs, delegates calculation to compute_basemean() and
  #    metadata extension to update_basemean_metadata(), then writes back via
  #    set_data().
  # --------------------------------------------------------------------------

  observeEvent(input$add_basemean, {
    debug_log("Add Basemean button clicked", 1)

    tryCatch({
      data <- tryCatch(get_data(), error = function(e) NULL)
      def  <- tryCatch(data_def(),  error = function(e) NULL)
      req(!is.null(data), !is.null(def))

      selected_type    <- input$abundance_type_basemean
      selected_samples <- input$sample_selection_basemean
      suffix           <- input$suffix_basemean

      if (is.null(selected_type) || length(selected_samples) == 0) {
        showNotification(
          "Please select at least one abundance type and one sample.",
          type = "error"
        )
        debug_log("Validation failed: no abundance type or samples selected", 1)
        return(NULL)
      }

      result <- compute_basemean(data, def, selected_type, selected_samples,
                                 suffix, debug_log)
      if (is.null(result)) {
        showNotification("No matching abundance columns found for selection.",
                         type = "error")
        return(NULL)
      }

      updated_def <- update_basemean_metadata(
        def, result$data, result$new_col, selected_type, selected_samples,
        debug_log
      )

      success <- tryCatch({
        set_data(result$data)
      }, error = function(e) {
        debug_log(paste("set_data() failed:", e$message), 1)
        showNotification(paste("Error updating data:", e$message), type = "error")
        FALSE
      })

      if (isTRUE(success)) {
        debug_log(paste("Basemean column added successfully:", result$new_col), 1)
        showNotification(
          paste("Basemean", result$new_col, "added successfully."),
          type = "message", duration = 3
        )
      } else {
        debug_log("set_data() returned FALSE after basemean calculation", 1)
        showNotification("Failed to apply Basemean update.", type = "error")
      }

    }, error = function(e) {
      debug_log(paste("Error in add_basemean handler:", e$message), 1)
      showNotification(paste("Error adding Basemean:", e$message),
                       type = "error", duration = 8)
    })
  })

  # --------------------------------------------------------------------------
  # d. Clear Basemeans button observer
  #    Removes all columns matching ^Basemean and their metadata rows, then
  #    writes back via set_data().
  # --------------------------------------------------------------------------

  observeEvent(input$clear_basemean, {
    debug_log("Clear Basemeans button clicked", 1)

    tryCatch({
      data <- tryCatch(get_data(), error = function(e) NULL)
      def  <- tryCatch(data_def(),  error = function(e) NULL)
      req(!is.null(data), !is.null(def))

      result <- clear_basemean_columns(data, def, debug_log)

      if (length(grep("^Basemean", names(data), value = TRUE)) == 0) {
        showNotification("No Basemean columns found.", type = "message")
        return(NULL)
      }

      success <- tryCatch({
        set_data(result$data)
      }, error = function(e) {
        debug_log(paste("set_data() failed during clear:", e$message), 1)
        showNotification(paste("Error clearing Basemean columns:", e$message),
                         type = "error")
        FALSE
      })

      if (isTRUE(success)) {
        debug_log("All Basemean columns cleared successfully", 1)
        showNotification("All Basemean columns cleared successfully.",
                         type = "message", duration = 3)
      }

    }, error = function(e) {
      debug_log(paste("Error in clear_basemean handler:", e$message), 1)
      showNotification(paste("Error clearing Basemean columns:", e$message),
                       type = "error", duration = 8)
    })
  })

  # --------------------------------------------------------------------------
  # e. UI config import observer
  #    Detects changes to the external UI_config and applies the basemean
  #    sub-configuration if present. Skips NULL configs and configs that do
  #    not contain basemean fields.
  # --------------------------------------------------------------------------

  observeEvent(get_ui_config(), {
    tryCatch({
      config <- get_ui_config()

      if (is.null(config)) {
        debug_log("UI config observer: no config detected — skipping", 2)
        return()
      }

      debug_log("UI config change detected — applying basemean configuration", 1)

      cfg <- NULL
      if (!is.null(config$basemean_configurations)) {
        cfg <- config$basemean_configurations
      } else if (all(c("abundance_type", "samples", "suffix") %in% names(config))) {
        cfg <- config
      }

      if (!is.null(cfg) && is.list(cfg)) {
        isolate({
          apply_ui_config(cfg)
        })
        debug_log("Basemean UI configuration applied from import", 1)
      } else {
        debug_log("UI config: no valid basemean fields found — skipping", 2)
      }

      current_ui_config(config)

    }, error = function(e) {
      debug_log(paste("Error in UI config import observer:", e$message), 1)
      showNotification(
        paste("Error applying Basemean configuration:", e$message),
        type = "error", duration = 6
      )
    })
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # f. UI config write-back observer
  #    Pushes the current input values back into UI_config when it is a
  #    reactiveVal (writeable). For read-only or static configs this is a
  #    no-op. Runs on every relevant input change.
  # --------------------------------------------------------------------------

  observe({
    if (ui_config_update_active()) return()

    new_cfg <- list(
      abundance_type = input$abundance_type_basemean,
      samples        = input$sample_selection_basemean,
      suffix         = input$suffix_basemean
    )

    if ("reactiveVal" %in% class(UI_config)) {
      isolate({
        UI_config(new_cfg)
      })
      debug_log("UI inputs written back to central UI_config (reactiveVal)", 2)
    }
  })
}
